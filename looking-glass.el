;;; looking-glass.el --- Profunctor optics for Emacs Lisp -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6"))
;; Keywords: lisp, extensions

;;; Commentary:

;; looking-glass provides a profunctor-based optics core for Emacs Lisp.

;;; Code:

(require 'cl-lib)

(define-error 'lg-no-focus "No focus found")
(define-error 'lg-expected-non-nil "Expected non-nil focus")

(cl-defstruct lg-monoid
  empty
  append)

(cl-defstruct lg-applicative
  pure
  ap
  fmap)

(cl-defstruct lg-identity
  value)

(cl-defstruct lg-const
  value)

(cl-defstruct lg-profunctor
  dimap
  first
  right
  wander)

(cl-defstruct lg-optic
  apply)

(defun lg-left (value)
  "Construct a left Either VALUE."
  (cons 'left value))

(defun lg-right (value)
  "Construct a right Either VALUE."
  (cons 'right value))

(defun lg-left-p (either)
  "Return non-nil when EITHER is a left value."
  (eq (car either) 'left))

(defun lg-right-p (either)
  "Return non-nil when EITHER is a right value."
  (eq (car either) 'right))

(defun lg--identity-applicative ()
  "Return the identity applicative."
  (make-lg-applicative
   :pure (lambda (value) (make-lg-identity :value value))
   :ap (lambda (wrapped-fn wrapped-value)
         (make-lg-identity
          :value
          (funcall (lg-identity-value wrapped-fn)
                   (lg-identity-value wrapped-value))))
   :fmap (lambda (fn wrapped-value)
           (make-lg-identity :value (funcall fn (lg-identity-value wrapped-value))))))

(defun lg--const-applicative (monoid)
  "Return a const applicative for MONOID."
  (let ((mempty (lg-monoid-empty monoid))
        (mappend (lg-monoid-append monoid)))
    (make-lg-applicative
     :pure (lambda (_value) (make-lg-const :value mempty))
     :ap (lambda (wrapped-fn wrapped-value)
           (make-lg-const
            :value
            (funcall mappend
                     (lg-const-value wrapped-fn)
                     (lg-const-value wrapped-value))))
     :fmap (lambda (_fn wrapped-value) wrapped-value))))

(defun lg--star-profunctor (applicative)
  "Return Star profunctor implementation over APPLICATIVE."
  (let ((fmap (lg-applicative-fmap applicative)))
    (make-lg-profunctor
     :dimap (lambda (before after pab)
              (lambda (value)
                (funcall fmap
                         after
                         (funcall pab (funcall before value)))))
     :first (lambda (pab)
              (lambda (pair)
                (let ((left (car pair))
                      (right (cdr pair)))
                  (funcall fmap
                           (lambda (new-left) (cons new-left right))
                           (funcall pab left)))))
     :right (lambda (pab)
              (lambda (either)
                (if (lg-right-p either)
                    (funcall fmap
                             (lambda (new-right) (lg-right new-right))
                             (funcall pab (cdr either)))
                  (funcall (lg-applicative-pure applicative)
                           (lg-left (cdr either))))))
     :wander (lambda (wander-fn pab)
               (lambda (value)
                 (funcall wander-fn pab value applicative))))))

(defun lg--run-optic (optic profunctor pab)
  "Run OPTIC against PROFUNCTOR using PAB."
  (funcall (lg-optic-apply optic) profunctor pab))

(defun lg-id ()
  "Return the identity optic."
  (make-lg-optic :apply (lambda (_p pab) pab)))

(defun lg-compose2 (outer inner)
  "Compose OUTER with INNER.
The resulting optic focuses through INNER first, then OUTER."
  (make-lg-optic
   :apply (lambda (p pab)
            (lg--run-optic inner p (lg--run-optic outer p pab)))))

(defun lg-compose (&rest optics)
  "Compose OPTICS from right to left."
  (if optics
      (cl-reduce #'lg-compose2 optics :from-end t)
    (lg-id)))

(defun lg-iso (forward backward)
  "Build an isomorphism from FORWARD and BACKWARD."
  (make-lg-optic
   :apply (lambda (p pab)
            (funcall (lg-profunctor-dimap p) forward backward pab))))

(defun lg-lens (getter setter)
  "Build a lens using GETTER and SETTER.
SETTER is called as (SETTER source new-focus)."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((first (lg-profunctor-first p)))
              (unless first
                (error "Lens requires profunctor first"))
              (funcall (lg-profunctor-dimap p)
                       (lambda (source)
                         (cons (funcall getter source) source))
                       (lambda (focused-and-source)
                         (funcall setter
                                  (cdr focused-and-source)
                                  (car focused-and-source)))
                       (funcall first pab))))))

(defun lg-prism (match build)
  "Build a prism from MATCH and BUILD.
MATCH must return (lg-left t) or (lg-right a).
BUILD maps b to t."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((right (lg-profunctor-right p)))
              (unless right
                (error "Prism requires profunctor right"))
              (funcall (lg-profunctor-dimap p)
                       match
                       (lambda (either)
                         (if (lg-right-p either)
                             (funcall build (cdr either))
                           (cdr either)))
                       (funcall right pab))))))

(defun lg-traversal (wander-fn)
  "Build a traversal from WANDER-FN.
WANDER-FN is called as (WANDER-FN afb source applicative)."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((wander (lg-profunctor-wander p)))
              (unless wander
                (error "Traversal requires profunctor wander"))
              (funcall wander wander-fn pab)))))

(defun lg--traverse-list (applicative afb source)
  "Traverse SOURCE list with AFB using APPLICATIVE."
  (let ((pure (lg-applicative-pure applicative))
        (ap (lg-applicative-ap applicative))
        (fmap (lg-applicative-fmap applicative)))
    (cl-reduce
     (lambda (acc focus)
       (funcall ap
                (funcall fmap
                         (lambda (partial)
                           (lambda (item) (append partial (list item))))
                         acc)
                (funcall afb focus)))
     source
     :initial-value (funcall pure nil))))

(defun lg--plist-present-and-value (plist key testfn)
  "Return (FOUND . VALUE) for KEY in PLIST using TESTFN."
  (let ((rest plist)
        found
        value)
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (when (and (not found) (funcall testfn candidate key))
          (setq found t
                value candidate-value))
        (setq rest (cddr rest))))
    (cons found value)))

(defun lg--plist-update-first (plist key value testfn)
  "Return PLIST with first KEY set to VALUE using TESTFN."
  (let ((rest plist)
        (result nil)
        (updated nil))
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (if (and (not updated) (funcall testfn candidate key))
            (progn
              (setq updated t)
              (setq result (append result (list candidate value))))
          (setq result (append result (list candidate candidate-value)))))
      (setq rest (cddr rest)))
    result))

(defun lg--alist-present-and-value (alist key testfn)
  "Return (FOUND . VALUE) for KEY in ALIST using TESTFN."
  (let ((cell (cl-find-if (lambda (entry)
                            (funcall testfn (car entry) key))
                          alist)))
    (if cell (cons t (cdr cell)) (cons nil nil))))

(defun lg--alist-update-first (alist key value testfn)
  "Return ALIST with first KEY set to VALUE using TESTFN."
  (let ((updated nil))
    (mapcar (lambda (entry)
              (if (and (not updated) (funcall testfn (car entry) key))
                  (progn
                    (setq updated t)
                    (cons (car entry) value))
                entry))
            alist)))

(defun lg--affine-traversal (preview-fn set-fn)
  "Build an affine traversal from PREVIEW-FN and SET-FN.
PREVIEW-FN must return (FOUND . FOCUS).
SET-FN is called as (SET-FN source new-focus)."
  (lg-traversal
   (lambda (afb source applicative)
     (let* ((result (funcall preview-fn source))
            (found (car result))
            (focus (cdr result))
            (pure (lg-applicative-pure applicative))
            (fmap (lg-applicative-fmap applicative)))
       (if found
           (funcall fmap
                    (lambda (new-focus)
                      (funcall set-fn source new-focus))
                    (funcall afb focus))
         (funcall pure source))))))

(defun lg-over (optic fn source)
  "Apply FN over OPTIC focus in SOURCE."
  (let* ((app (lg--identity-applicative))
         (profunctor (lg--star-profunctor app))
         (transform (lg--run-optic
                     optic
                     profunctor
                     (lambda (focus)
                       (make-lg-identity :value (funcall fn focus))))))
    (lg-identity-value (funcall transform source))))

(defun lg-set (optic value source)
  "Set OPTIC focus to VALUE in SOURCE."
  (lg-over optic (lambda (_focus) value) source))

(defun lg-to-list-of (optic source)
  "Collect all focus values for OPTIC in SOURCE."
  (let* ((monoid (make-lg-monoid :empty nil :append #'append))
         (app (lg--const-applicative monoid))
         (profunctor (lg--star-profunctor app))
         (transform (lg--run-optic
                     optic
                     profunctor
                     (lambda (focus)
                       (make-lg-const :value (list focus))))))
    (lg-const-value (funcall transform source))))

(defun lg-preview-result (optic source)
  "Return a disambiguated preview for OPTIC in SOURCE.
Returns (FOUND . VALUE), where FOUND is non-nil iff a focus exists.
VALUE can be nil when nil is an actual focus value."
  (let ((focuses (lg-to-list-of optic source)))
    (if focuses
        (cons t (car focuses))
      (cons nil nil))))

(defun lg-preview (optic source)
  "Return first focus value for OPTIC in SOURCE, or nil when absent."
  (cdr (lg-preview-result optic source)))

(defun lg-has (optic source)
  "Return non-nil when OPTIC has at least one focus in SOURCE."
  (car (lg-preview-result optic source)))

(defun lg-view (optic source)
  "View exactly one expected focus from OPTIC in SOURCE.
Signals `lg-no-focus' when no focus exists."
  (let ((result (lg-preview-result optic source)))
    (if (car result)
        (cdr result)
      (signal 'lg-no-focus (list optic source)))))

(defun lg-view-non-nil (optic source)
  "View focus with OPTIC in SOURCE and require a non-nil result."
  (let ((value (lg-view optic source)))
    (if value
        value
      (signal 'lg-expected-non-nil (list optic source)))))

(defun lg-non-nil ()
  "A prism that focuses a non-nil value."
  (lg-prism
   (lambda (value)
     (if value (lg-right value) (lg-left nil)))
   #'identity))

(defun lg-car-lens ()
  "Lens focusing car of a cons cell."
  (lg-lens #'car (lambda (source new-focus) (cons new-focus (cdr source)))))

(defun lg-cdr-lens ()
  "Lens focusing cdr of a cons cell."
  (lg-lens #'cdr (lambda (source new-focus) (cons (car source) new-focus))))

(defun lg-list-traversal ()
  "Traversal over all elements in a list."
  (lg-traversal
   (lambda (afb source applicative)
     (lg--traverse-list applicative afb source))))

(defun lg-vector-traversal ()
  "Traversal over all elements in a vector."
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                #'vconcat
                (lg--traverse-list applicative afb (append source nil)))))))

(defun lg-string-traversal ()
  "Traversal over all characters in a string."
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                (lambda (parts) (apply #'concat parts))
                (lg--traverse-list
                 applicative
                 (lambda (character)
                   (funcall afb (char-to-string character)))
                 (string-to-list source)))))))

(defun lg-nth-lens (index)
  "Lens focusing INDEX in a list.
Signals when INDEX is out of range."
  (lg-lens
   (lambda (source)
     (unless (and (>= index 0) (< index (length source)))
       (error "Index %s out of range" index))
     (nth index source))
   (lambda (source new-focus)
     (unless (and (>= index 0) (< index (length source)))
       (error "Index %s out of range" index))
     (let ((result (copy-sequence source)))
       (setf (nth index result) new-focus)
       result))))

(defun lg-plist-key-traversal (key &optional testfn)
  "Affine traversal focusing KEY in a plist.
TESTFN defaults to `eq'."
  (let ((compare (or testfn #'eq)))
    (lg--affine-traversal
     (lambda (source)
       (lg--plist-present-and-value source key compare))
     (lambda (source new-focus)
       (lg--plist-update-first source key new-focus compare)))))

(defun lg-alist-key-traversal (key &optional testfn)
  "Affine traversal focusing KEY in an alist.
TESTFN defaults to `equal'."
  (let ((compare (or testfn #'equal)))
    (lg--affine-traversal
     (lambda (source)
       (lg--alist-present-and-value source key compare))
     (lambda (source new-focus)
       (lg--alist-update-first source key new-focus compare)))))

(defun lg-just ()
  "A prism for optional values represented as (just VALUE) or nil."
  (lg-prism
   (lambda (value)
     (if (and (consp value) (eq (car value) 'just))
         (lg-right (cdr value))
       (lg-left value)))
   (lambda (value)
     (cons 'just value))))

(provide 'looking-glass)

;;; looking-glass.el ends here
