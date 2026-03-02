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

(cl-defstruct lg-indexed-profunctor
  dimap
  first
  right
  wander
  reindex)

(cl-defstruct lg-optic
  apply
  review)

(cl-defstruct lg-indexed-optic
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

(defun lg--indexed-star-profunctor (applicative)
  "Return Star profunctor for indexed optics over APPLICATIVE.
The mapping function receives (INDEX FOCUS)."
  (let ((fmap (lg-applicative-fmap applicative)))
    (make-lg-indexed-profunctor
     :dimap (lambda (before after pib)
              (lambda (value)
                (funcall fmap
                         after
                         (funcall pib nil (funcall before value)))))
     :first (lambda (pib)
              (lambda (pair)
                (let ((left (car pair))
                      (right (cdr pair)))
                  (funcall fmap
                           (lambda (new-left) (cons new-left right))
                           (funcall pib nil left)))))
     :right (lambda (pib)
              (lambda (either)
                (if (lg-right-p either)
                    (funcall fmap
                             (lambda (new-right) (lg-right new-right))
                             (funcall pib nil (cdr either)))
                  (funcall (lg-applicative-pure applicative)
                           (lg-left (cdr either))))))
     :wander (lambda (wander-fn pib)
               (lambda (value)
                 (funcall wander-fn pib value applicative)))
     :reindex (lambda (index-fn pib)
                (lambda (index focus)
                  (funcall pib (funcall index-fn index) focus))))))

(defun lg--run-optic (optic profunctor pab)
  "Run OPTIC against PROFUNCTOR using PAB."
  (funcall (lg-optic-apply optic) profunctor pab))

(defun lg--run-indexed-optic (optic indexed-profunctor pib)
  "Run indexed OPTIC against INDEXED-PROFUNCTOR using PIB."
  (funcall (lg-indexed-optic-apply optic) indexed-profunctor pib))

(defun lg-id ()
  "Return the identity optic."
  (make-lg-optic :apply (lambda (_p pab) pab)
                 :review #'identity))

(defun lg-indexed-id ()
  "Return the identity indexed optic."
  (make-lg-indexed-optic :apply (lambda (_p pib) pib)))

(defun lg-compose2 (outer inner)
  "Compose OUTER with INNER.
The resulting optic focuses through INNER first, then OUTER."
  (make-lg-optic
   :apply (lambda (p pab)
            (lg--run-optic inner p (lg--run-optic outer p pab)))
   :review (let ((outer-review (lg-optic-review outer))
                 (inner-review (lg-optic-review inner)))
             (when (and outer-review inner-review)
               (lambda (value)
                 (funcall outer-review (funcall inner-review value)))))))

(defun lg-compose (&rest optics)
  "Compose OPTICS from right to left."
  (if optics
      (cl-reduce #'lg-compose2 optics :from-end t)
    (lg-id)))

(defun lg-compose-indexed2 (outer inner)
  "Compose indexed OUTER with indexed INNER."
  (make-lg-indexed-optic
   :apply (lambda (p pib)
            (lg--run-indexed-optic inner p (lg--run-indexed-optic outer p pib)))))

(defun lg-compose-indexed (&rest optics)
  "Compose indexed OPTICS from right to left."
  (if optics
      (cl-reduce #'lg-compose-indexed2 optics :from-end t)
    (lg-indexed-id)))

(defun lg-iso (forward backward)
  "Build an isomorphism from FORWARD and BACKWARD."
  (make-lg-optic
   :apply (lambda (p pab)
            (funcall (lg-profunctor-dimap p) forward backward pab))
   :review backward))

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
                       (funcall right pab))))
   :review build))

(defun lg-review (optic value)
  "Construct a target value from VALUE using OPTIC review.
Signals when OPTIC does not support review."
  (let ((reviewer (lg-optic-review optic)))
    (if reviewer
        (funcall reviewer value)
      (error "Optic does not support review"))))

(defun lg-traversal (wander-fn)
  "Build a traversal from WANDER-FN.
WANDER-FN is called as (WANDER-FN afb source applicative)."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((wander (lg-profunctor-wander p)))
              (unless wander
                (error "Traversal requires profunctor wander"))
              (funcall wander wander-fn pab)))))

(defun lg-indexed-traversal (wander-fn)
  "Build an indexed traversal from WANDER-FN.
WANDER-FN is called as (WANDER-FN iafb source applicative), where
IAFB is called as (IAFB index focus)."
  (make-lg-indexed-optic
   :apply (lambda (p pib)
            (let ((wander (lg-indexed-profunctor-wander p)))
              (unless wander
                (error "Indexed traversal requires indexed profunctor wander"))
              (funcall wander wander-fn pib)))))

(defun lg-ireindexed (index-fn optic)
  "Transform OPTIC indices with INDEX-FN."
  (make-lg-indexed-optic
   :apply (lambda (p pib)
            (let ((reindex (lg-indexed-profunctor-reindex p)))
              (unless reindex
                (error "Indexed optic requires indexed profunctor reindex"))
              (lg--run-indexed-optic optic p (funcall reindex index-fn pib))))))

(defun lg-getter (getter-fn)
  "Build a read-only getter optic from GETTER-FN.
Setter-like operations leave the source unchanged."
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                (lambda (_new-focus) source)
                (funcall afb (funcall getter-fn source)))))))

(defalias 'lg-to #'lg-getter)

(defun lg-filtered (predicate)
  "Traversal that focuses only values satisfying PREDICATE."
  (lg-traversal
   (lambda (afb source applicative)
     (if (funcall predicate source)
         (funcall afb source)
       (funcall (lg-applicative-pure applicative) source)))))

(defun lg-ifiltered (predicate)
  "Indexed traversal that focuses only values satisfying PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (lg-indexed-traversal
   (lambda (iafb source applicative)
     (if (funcall predicate nil source)
         (funcall iafb nil source)
       (funcall (lg-applicative-pure applicative) source)))))

(defun lg-indices (predicate)
  "Indexed traversal that focuses when index matches PREDICATE."
  (lg-ifiltered (lambda (index _focus) (funcall predicate index))))

(defun lg-indexed-list-filtered (predicate)
  "Indexed list traversal focused by PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (lg-indexed-traversal
   (lambda (iafb source applicative)
     (lg--traverse-list-indexed
      applicative
      (lambda (index focus)
        (if (funcall predicate index focus)
            (funcall iafb index focus)
          (funcall (lg-applicative-pure applicative) focus)))
      source))))

(defun lg-indexed-list-indices (predicate)
  "Indexed list traversal focused by index PREDICATE."
  (lg-indexed-list-filtered
   (lambda (index _focus)
     (funcall predicate index))))

(defun lg-required (optic)
  "Require non-nil focus for OPTIC by composing with `lg-non-nil'."
  (lg-compose (lg-non-nil) optic))

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

(defun lg--traverse-list-indexed (applicative iafb source)
  "Traverse SOURCE list with IAFB using APPLICATIVE.
IAFB is called as (IAFB index focus)."
  (let ((pure (lg-applicative-pure applicative))
        (ap (lg-applicative-ap applicative))
        (fmap (lg-applicative-fmap applicative))
        (index 0))
    (cl-reduce
     (lambda (acc focus)
       (let ((current index))
         (setq index (1+ index))
         (funcall ap
                  (funcall fmap
                           (lambda (partial)
                             (lambda (item) (append partial (list item))))
                           acc)
                  (funcall iafb current focus))))
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

(defun lg--plist-remove-first (plist key testfn)
  "Return PLIST with first KEY removed using TESTFN."
  (let ((rest plist)
        (result nil)
        (removed nil))
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (if (and (not removed) (funcall testfn candidate key))
            (setq removed t)
          (setq result (append result (list candidate candidate-value)))))
      (setq rest (cddr rest)))
    result))

(defun lg--plist-set-first-or-add (plist key value testfn)
  "Return PLIST with first KEY set to VALUE, adding when missing."
  (let ((found (car (lg--plist-present-and-value plist key testfn))))
    (if found
        (lg--plist-update-first plist key value testfn)
      (append plist (list key value)))))

(defun lg--alist-remove-first (alist key testfn)
  "Return ALIST with first KEY removed using TESTFN."
  (let ((result nil)
        (removed nil))
    (dolist (entry alist (nreverse result))
      (if (and (not removed) (funcall testfn (car entry) key))
          (setq removed t)
        (push entry result)))))

(defun lg--alist-set-first-or-add (alist key value testfn)
  "Return ALIST with first KEY set to VALUE, adding when missing."
  (let ((found (car (lg--alist-present-and-value alist key testfn))))
    (if found
        (lg--alist-update-first alist key value testfn)
      (append alist (list (cons key value))))))

(defun lg--alist-p (value)
  "Return non-nil when VALUE is an alist."
  (and (listp value)
       (cl-every #'consp value)))

(defun lg--keyed-kind (source)
  "Classify SOURCE as one supported keyed container kind."
  (cond
   ((hash-table-p source) 'hash-table)
   ((lg--alist-p source) 'alist)
   ((and (listp source) (zerop (% (length source) 2))) 'plist)
   (t nil)))

(defun lg--keyed-present-and-value (source key &optional testfn)
  "Return (FOUND . VALUE) for KEY in SOURCE.
SOURCE can be plist, alist, or hash table."
  (pcase (lg--keyed-kind source)
    ('plist (lg--plist-present-and-value source key (or testfn #'eq)))
    ('alist (lg--alist-present-and-value source key (or testfn #'equal)))
    ('hash-table
     (let* ((missing (make-symbol "lg-missing"))
            (value (gethash key source missing)))
       (if (eq value missing)
           (cons nil nil)
         (cons t value))))
    (_ (error "Unsupported keyed source type: %S" source))))

(defun lg--keyed-set-existing (source key value &optional testfn)
  "Set existing KEY to VALUE in SOURCE, preserving missing keys."
  (pcase (lg--keyed-kind source)
    ('plist
     (if (car (lg--plist-present-and-value source key (or testfn #'eq)))
         (lg--plist-update-first source key value (or testfn #'eq))
       source))
    ('alist
     (if (car (lg--alist-present-and-value source key (or testfn #'equal)))
         (lg--alist-update-first source key value (or testfn #'equal))
       source))
    ('hash-table
     (let* ((missing (make-symbol "lg-missing"))
            (current (gethash key source missing)))
       (if (eq current missing)
           source
         (let ((copy (copy-hash-table source)))
           (puthash key value copy)
           copy))))
    (_ (error "Unsupported keyed source type: %S" source))))

(defun lg--keyed-set-presence (source key present value &optional testfn)
  "Set presence state for KEY in SOURCE.
When PRESENT is non-nil, insert or update KEY with VALUE.
When PRESENT is nil, remove KEY if present."
  (pcase (lg--keyed-kind source)
    ('plist
     (if present
         (lg--plist-set-first-or-add source key value (or testfn #'eq))
       (lg--plist-remove-first source key (or testfn #'eq))))
    ('alist
     (if present
         (lg--alist-set-first-or-add source key value (or testfn #'equal))
       (lg--alist-remove-first source key (or testfn #'equal))))
    ('hash-table
     (let ((copy (copy-hash-table source)))
       (if present
           (puthash key value copy)
         (remhash key copy))
       copy))
    (_ (error "Unsupported keyed source type: %S" source))))

(defun lg-affine-traversal (preview-fn set-fn)
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

(defalias 'lg--affine-traversal #'lg-affine-traversal)

(defun lg-ix (key &optional testfn)
  "Affine traversal focusing existing KEY in keyed SOURCE.
SOURCE can be plist, alist, or hash table.
TESTFN applies to plist/alist key comparisons."
  (lg-affine-traversal
   (lambda (source)
     (lg--keyed-present-and-value source key testfn))
   (lambda (source new-focus)
     (lg--keyed-set-existing source key new-focus testfn))))

(defun lg-at (key &optional testfn)
  "Lens focusing presence and value of KEY in keyed SOURCE.
The focus shape is (FOUND . VALUE). Setting to (nil . _) removes KEY.
Setting to (t . VALUE) inserts or updates KEY.
SOURCE can be plist, alist, or hash table.
TESTFN applies to plist/alist key comparisons."
  (lg-lens
   (lambda (source)
     (lg--keyed-present-and-value source key testfn))
   (lambda (source state)
     (unless (consp state)
       (error "lg-at expects state of shape (FOUND . VALUE)"))
     (lg--keyed-set-presence source key (car state) (cdr state) testfn))))

(defun lg-hash-key-traversal (key)
  "Affine traversal focusing existing KEY in a hash table."
  (lg-ix key))

(defun lg-hash-key-at (key)
  "Lens focusing presence and value for KEY in a hash table."
  (lg-at key))

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

(defun lg-iover (optic fn source)
  "Apply indexed FN over OPTIC focus in SOURCE.
FN is called as (FN index focus)."
  (let* ((app (lg--identity-applicative))
         (profunctor (lg--indexed-star-profunctor app))
         (transform (lg--run-indexed-optic
                     optic
                     profunctor
                     (lambda (index focus)
                       (make-lg-identity
                        :value (funcall fn index focus))))))
    (lg-identity-value (funcall transform source))))

(defun lg-set (optic value source)
  "Set OPTIC focus to VALUE in SOURCE."
  (lg-over optic (lambda (_focus) value) source))

(defun lg-foldl-of (optic fn initial source)
  "Left-fold OPTIC focuses in SOURCE with FN and INITIAL."
  (cl-reduce fn (lg-to-list-of optic source) :initial-value initial))

(defun lg-first-of (optic source)
  "Return first focus of OPTIC in SOURCE, or nil when absent."
  (cdr (lg-preview-result optic source)))

(defun lg-last-of (optic source)
  "Return last focus of OPTIC in SOURCE, or nil when absent."
  (let ((focuses (lg-to-list-of optic source)))
    (when focuses
      (car (last focuses)))))

(defun lg-find-of (optic predicate source)
  "Return first focus matching PREDICATE for OPTIC in SOURCE."
  (cl-find-if predicate (lg-to-list-of optic source)))

(defun lg-ifoldl-of (optic fn initial source)
  "Left-fold indexed OPTIC focuses in SOURCE with FN and INITIAL.
FN is called as (FN acc index focus)."
  (cl-reduce (lambda (acc indexed-focus)
               (funcall fn acc (car indexed-focus) (cdr indexed-focus)))
             (lg-ito-list-of optic source)
             :initial-value initial))

(defun lg-ifirst-of (optic source)
  "Return first indexed focus of OPTIC in SOURCE, or nil when absent."
  (cdr (lg-ipreview-result optic source)))

(defun lg-ilast-of (optic source)
  "Return last indexed focus of OPTIC in SOURCE, or nil when absent."
  (let ((focuses (lg-ito-list-of optic source)))
    (when focuses
      (car (last focuses)))))

(defun lg-ifind-of (optic predicate source)
  "Return first indexed focus matching PREDICATE in SOURCE.
PREDICATE is called as (PREDICATE index focus)."
  (cl-find-if (lambda (indexed-focus)
                (funcall predicate (car indexed-focus) (cdr indexed-focus)))
              (lg-ito-list-of optic source)))

(defun lg-imap-of (optic fn source)
  "Indexed map over OPTIC in SOURCE using FN.
FN is called as (FN index focus)."
  (lg-iover optic fn source))

(defun lg-any-of (optic predicate source)
  "Return non-nil when any OPTIC focus in SOURCE satisfies PREDICATE."
  (cl-some predicate (lg-to-list-of optic source)))

(defun lg-none-of (optic predicate source)
  "Return non-nil when no OPTIC focus in SOURCE satisfies PREDICATE."
  (not (lg-any-of optic predicate source)))

(defun lg-all-of (optic predicate source)
  "Return non-nil when all OPTIC focuses in SOURCE satisfy PREDICATE."
  (cl-every predicate (lg-to-list-of optic source)))

(defun lg-iany-of (optic predicate source)
  "Return non-nil when any indexed focus satisfies PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (cl-some (lambda (indexed-focus)
             (funcall predicate (car indexed-focus) (cdr indexed-focus)))
           (lg-ito-list-of optic source)))

(defun lg-inone-of (optic predicate source)
  "Return non-nil when no indexed focus satisfies PREDICATE."
  (not (lg-iany-of optic predicate source)))

(defun lg-iall-of (optic predicate source)
  "Return non-nil when all indexed focuses satisfy PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (cl-every (lambda (indexed-focus)
              (funcall predicate (car indexed-focus) (cdr indexed-focus)))
            (lg-ito-list-of optic source)))

(defun lg-count-of (optic source)
  "Count focused values of OPTIC in SOURCE."
  (length (lg-to-list-of optic source)))

(defun lg-icount-of (optic source)
  "Count focused values of indexed OPTIC in SOURCE."
  (length (lg-ito-list-of optic source)))

(defun lg-unto (builder)
  "Create a review optic from BUILDER.
BUILDER maps focus-domain values into source-domain values."
  (make-lg-optic
   :apply (lambda (_p pab) pab)
   :review builder))

(defun lg-reviews (optic fn value)
  "Review VALUE through OPTIC, then apply FN to the result."
  (funcall fn (lg-review optic value)))

(defun lg-preview-or (default optic source)
  "Preview OPTIC in SOURCE, returning DEFAULT only when missing."
  (let ((result (lg-preview-result optic source)))
    (if (car result) (cdr result) default)))

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

(defun lg-ito-list-of (optic source)
  "Collect all indexed focus values for OPTIC in SOURCE.
Each item is (INDEX . FOCUS)."
  (let* ((monoid (make-lg-monoid :empty nil :append #'append))
         (app (lg--const-applicative monoid))
         (profunctor (lg--indexed-star-profunctor app))
         (transform (lg--run-indexed-optic
                     optic
                     profunctor
                     (lambda (index focus)
                       (make-lg-const :value (list (cons index focus)))))))
    (lg-const-value (funcall transform source))))

(defun lg-ipreview-result (optic source)
  "Return a disambiguated indexed preview for OPTIC in SOURCE.
Returns (FOUND . (INDEX . VALUE))."
  (let ((focuses (lg-ito-list-of optic source)))
    (if focuses
        (cons t (car focuses))
      (cons nil nil))))

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

(defmacro lg-optic (&rest optics)
  "Compose OPTICS at macro expansion time."
  `(lg-compose ,@optics))

(defmacro lg-over-> (source &rest steps)
  "Thread SOURCE through `lg-over' steps.
Each step is (OPTIC FN)."
  (let ((result source))
    (dolist (step steps result)
      (let ((optic (car step))
            (fn (cadr step)))
        (setq result `(lg-over ,optic ,fn ,result))))))

(defmacro lg-set-> (source &rest steps)
  "Thread SOURCE through `lg-set' steps.
Each step is (OPTIC VALUE)."
  (let ((result source))
    (dolist (step steps result)
      (let ((optic (car step))
            (value (cadr step)))
        (setq result `(lg-set ,optic ,value ,result))))))

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

(defun lg-indexed-list-traversal ()
  "Indexed traversal over all elements in a list."
  (lg-indexed-traversal
   (lambda (iafb source applicative)
     (lg--traverse-list-indexed applicative iafb source))))

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
  (lg-ix key (or testfn #'eq)))

(defun lg-alist-key-traversal (key &optional testfn)
  "Affine traversal focusing KEY in an alist.
TESTFN defaults to `equal'."
  (lg-ix key (or testfn #'equal)))

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
