;;; looking-glass.el --- Composable optics for Emacs Lisp -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.6"))
;; Keywords: lisp, data, tools
;; URL: https://example.invalid/looking-glass

;;; Commentary:

;; looking-glass provides a small, composable optics library inspired by
;; Haskell's lens ecosystem, adapted to common Emacs Lisp data structures.
;;
;; Optics are represented as first-class values and can be composed with
;; `lg-compose'.
;;
;; Core operations:
;; - `lg-view': extract a focused value (single-focus optics)
;; - `lg-preview': extract the first focus (partial/multi-focus optics)
;; - `lg-to-list-of': collect all foci
;; - `lg-set': replace focused values
;; - `lg-over': transform focused values
;; - `lg-review': build a source value from a prism

;;; Code:

(require 'cl-lib)

(cl-defstruct (lg-optic
               (:constructor lg--optic-create))
  "Internal representation of an optic.

KIND is one of `lens', `prism', or `traversal'.
OVER is a function (FN SOURCE) -> SOURCE.
TO-LIST is a function (SOURCE) -> list of foci.
REVIEW is optional and is used by prisms to construct SOURCE from a focus."
  kind
  over
  to-list
  review)

(defun lg--ensure-optic (value)
  "Signal an error unless VALUE is an `lg-optic'."
  (unless (lg-optic-p value)
    (signal 'wrong-type-argument (list 'lg-optic-p value))))

(defun lg-lens (getter setter)
  "Build a lens from GETTER and SETTER.

GETTER is a function of one argument SOURCE returning the focus.
SETTER is a function of two arguments FOCUS and SOURCE returning updated SOURCE."
  (lg--optic-create
   :kind 'lens
   :to-list (lambda (source)
              (list (funcall getter source)))
   :over (lambda (fn source)
           (funcall setter
                    (funcall fn (funcall getter source))
                    source))))

(defun lg-prism (matcher reviewer)
  "Build a prism from MATCHER and REVIEWER.

MATCHER is a function of one argument SOURCE.  It should return nil for no
match, or a cons cell (t . FOCUS) for match.

REVIEWER is a function of one argument FOCUS that constructs SOURCE."
  (lg--optic-create
   :kind 'prism
   :to-list (lambda (source)
              (let ((match (funcall matcher source)))
                (if (and (consp match) (car match))
                    (list (cdr match))
                  nil)))
   :over (lambda (fn source)
           (let ((match (funcall matcher source)))
             (if (and (consp match) (car match))
                 (funcall reviewer (funcall fn (cdr match)))
               source)))
   :review reviewer))

(defun lg-traversal (over to-list)
  "Build a traversal from OVER and TO-LIST.

OVER is a function (FN SOURCE) -> SOURCE.
TO-LIST is a function (SOURCE) -> list of foci."
  (lg--optic-create
   :kind 'traversal
   :over over
   :to-list to-list))

(defun lg-compose (&rest optics)
  "Compose OPTICS left-to-right.

When OPTICS is empty, return `lg-id'."
  (if (null optics)
      (lg-id)
    (progn
      (mapc #'lg--ensure-optic optics)
      (let ((composed (car optics)))
        (dolist (optic (cdr optics))
          (setq composed
                (let ((outer composed)
                      (inner optic))
                  (lg--optic-create
                   :kind (if (eq (lg-optic-kind outer) 'lens)
                             (lg-optic-kind inner)
                           'traversal)
                   :over (lambda (fn source)
                           (funcall (lg-optic-over outer)
                                    (lambda (focus)
                                      (funcall (lg-optic-over inner) fn focus))
                                    source))
                   :to-list (lambda (source)
                              (cl-mapcan (lambda (focus)
                                           (funcall (lg-optic-to-list inner)
                                                    focus))
                                         (funcall (lg-optic-to-list outer)
                                                  source)))
                   :review (when (and (lg-optic-review outer)
                                      (lg-optic-review inner))
                             (lambda (value)
                               (funcall (lg-optic-review outer)
                                        (funcall (lg-optic-review inner)
                                                 value))))))))
        composed))))

(defun lg-id ()
  "Identity lens that focuses SOURCE itself."
  (lg-lens #'identity (lambda (new _source) new)))

(defun lg-over (optic fn source)
  "Apply FN to each focus of OPTIC in SOURCE."
  (lg--ensure-optic optic)
  (funcall (lg-optic-over optic) fn source))

(defun lg-set (optic value source)
  "Replace each focus of OPTIC in SOURCE with VALUE."
  (lg-over optic (lambda (_focus) value) source))

(defun lg-to-list-of (optic source)
  "Collect all foci of OPTIC in SOURCE."
  (lg--ensure-optic optic)
  (funcall (lg-optic-to-list optic) source))

(defun lg-view (optic source)
  "Extract a single focus from OPTIC in SOURCE.

Signal an error when OPTIC does not focus exactly one value."
  (let ((foci (lg-to-list-of optic source)))
    (pcase foci
      (`(,value) value)
      (_ (error "`lg-view' expected exactly one focus, got %d"
                (length foci))))))

(defun lg-preview (optic source)
  "Extract the first focus from OPTIC in SOURCE, or nil."
  (car (lg-to-list-of optic source)))

(defun lg-review (optic value)
  "Construct a source value by reviewing VALUE with OPTIC.

This works for prisms and compositions with review-capable optics."
  (lg--ensure-optic optic)
  (let ((reviewer (lg-optic-review optic)))
    (unless reviewer
      (error "Optic does not support `lg-review'"))
    (funcall reviewer value)))

(defun lg-nth (index)
  "Lens focusing INDEX in a proper list."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-lens
   (lambda (source)
     (nth index source))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (setf (nth index copy) new)
       copy))))

(defconst lg-car
  (lg-lens #'car (lambda (new source) (cons new (cdr source))))
  "Lens focusing the `car' of a cons cell.")

(defconst lg-cdr
  (lg-lens #'cdr (lambda (new source) (cons (car source) new)))
  "Lens focusing the `cdr' of a cons cell.")

(defun lg-gethash (key &optional test)
  "Lens focusing KEY in a hash table.

When TEST is non-nil, newly copied tables use that equality test."
  (lg-lens
   (lambda (source)
     (gethash key source))
   (lambda (new source)
     (let ((table (copy-hash-table source)))
       (when test
         (let ((rehydrated (make-hash-table :test test)))
           (maphash (lambda (k v) (puthash k v rehydrated)) table)
           (setq table rehydrated)))
       (puthash key new table)
       table))))

(defun lg-plist (prop)
  "Lens focusing PROP in a plist."
  (lg-lens
   (lambda (source)
     (plist-get source prop))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (plist-put copy prop new)
       copy))))

(defun lg-each-list ()
  "Traversal focusing each element in a list."
  (lg-traversal
   (lambda (fn source)
     (mapcar fn source))
   (lambda (source)
     (copy-sequence source))))

(defun lg-each-vector ()
  "Traversal focusing each element in a vector."
  (lg-traversal
   (lambda (fn source)
     (let ((copy (copy-sequence source)))
       (dotimes (i (length copy))
         (aset copy i (funcall fn (aref copy i))))
       copy))
   (lambda (source)
     (append source nil))))

(defun lg-just ()
  "Prism that matches non-nil values and reviews to themselves."
  (lg-prism
   (lambda (source)
     (when source
       (cons t source)))
   #'identity))

(defun lg-pred (predicate)
  "Prism that matches values satisfying PREDICATE."
  (lg-prism
   (lambda (source)
     (when (funcall predicate source)
       (cons t source)))
   #'identity))

(defun lg--regex-step (regexp string start)
  "Return match information for REGEXP in STRING at START.

The return value is nil when no match exists, or a list
(MATCH-START MATCH-END NEXT-START)."
  (when (string-match regexp string start)
    (let* ((mstart (match-beginning 0))
           (mend (match-end 0))
           (next (if (= start mend)
                     (min (1+ start) (length string))
                   mend)))
      (list mstart mend next))))

(defun lg--regex-to-list (regexp group source)
  "Collect regex foci for REGEXP and GROUP in SOURCE."
  (let ((cursor 0)
        (out nil))
    (while (< cursor (length source))
      (let ((step (lg--regex-step regexp source cursor)))
        (if (null step)
            (setq cursor (length source))
          (pcase-let ((`(,_mstart _mend ,next) step))
            (when (and (match-beginning group)
                       (match-end group))
              (push (match-string group source) out))
            (setq cursor next)))))
    (nreverse out)))

(defun lg--regex-over (regexp group fn source)
  "Apply FN to regex foci for REGEXP and GROUP in SOURCE."
  (let ((scan 0)
        (cursor 0)
        (pieces nil))
    (while (< scan (length source))
      (let ((step (lg--regex-step regexp source scan)))
        (if (null step)
            (progn
              (push (substring source cursor) pieces)
              (setq scan (length source)))
          (pcase-let ((`(,_mstart ,mend ,next) step))
            (let ((gstart (match-beginning group))
                  (gend (match-end group)))
              (if (or (null gstart) (null gend))
                  (progn
                    (push (substring source cursor mend) pieces)
                    (setq cursor mend))
                (push (substring source cursor gstart) pieces)
                (push (funcall fn (substring source gstart gend)) pieces)
                (push (substring source gend mend) pieces)
                (setq cursor mend)))
            (setq scan next)))))
    (apply #'concat (nreverse pieces))))

(defun lg-regex (regexp &optional group)
  "Traversal over REGEXP matches in strings.

GROUP defaults to 0 (full match).  When GROUP is non-zero, only that capture
group is focused and rewritten."
  (let ((target-group (or group 0)))
    (lg-traversal
     (lambda (fn source)
       (unless (stringp source)
         (error "`lg-regex' expects SOURCE to be a string"))
       (lg--regex-over regexp target-group fn source))
     (lambda (source)
       (unless (stringp source)
         (error "`lg-regex' expects SOURCE to be a string"))
       (lg--regex-to-list regexp target-group source)))))

(provide 'looking-glass)

;;; looking-glass.el ends here
