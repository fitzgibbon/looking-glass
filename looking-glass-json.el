;;; looking-glass-json.el --- JSON optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6") (looking-glass "0.1.0"))
;; Keywords: lisp, json, extensions

;;; Commentary:

;; JSON-oriented optics built on top of looking-glass core primitives.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'looking-glass)

(defconst lg-json-null 'lg-json-null
  "Default sentinel used to represent JSON null values.")

(defconst lg-json-false :false
  "Default sentinel used to represent JSON false values.")

(defcustom lg-json-object-type 'alist
  "Default object representation used by JSON parsing optics."
  :type '(choice (const :tag "Alist" alist)
                 (const :tag "Plist" plist)
                 (const :tag "Hash table" hash-table))
  :group 'lisp)

(defcustom lg-json-array-type 'array
  "Default array representation used by JSON parsing optics."
  :type '(choice (const :tag "List" list)
                 (const :tag "Array (vector)" array))
  :group 'lisp)

(defcustom lg-json-null-object lg-json-null
  "Default object used for JSON null values when parsing and rendering."
  :type 'sexp
  :group 'lisp)

(defcustom lg-json-false-object lg-json-false
  "Default object used for JSON false values when parsing and rendering."
  :type 'sexp
  :group 'lisp)

(defun lg-json--alist-p (value)
  "Return non-nil when VALUE is an alist."
  (and (listp value)
       (cl-every #'consp value)))

(defun lg-json--object-kind (value)
  "Classify VALUE as one supported JSON object representation."
  (cond
   ((hash-table-p value) 'hash-table)
   ((lg-json--alist-p value) 'alist)
   ((and (listp value) (zerop (% (length value) 2))) 'plist)
   (t nil)))

(defun lg-json--array-p (value)
  "Return non-nil when VALUE is a list or vector."
  (or (listp value) (vectorp value)))

(defun lg-json--present-object-key (object key testfn)
  "Return tagged maybe for KEY in OBJECT using TESTFN."
  (pcase (lg-json--object-kind object)
    ('hash-table
     (let* ((missing (make-symbol "lg-json-missing"))
            (value (gethash key object missing)))
       (if (eq value missing) lg-nothing (lg-just value))))
    ('alist
     (let ((cell (cl-find-if (lambda (entry)
                               (funcall testfn (car entry) key))
                             object)))
       (if cell (lg-just (cdr cell)) lg-nothing)))
    ('plist
     (let ((rest object)
           found
           value)
       (while rest
         (let ((candidate (car rest))
               (candidate-value (cadr rest)))
           (when (and (not found) (funcall testfn candidate key))
             (setq found t
                   value candidate-value))
           (setq rest (cddr rest))))
       (if found (lg-just value) lg-nothing)))
    (_ (error "Expected JSON object representation"))))

(defun lg-json--set-existing-object-key (object key value testfn)
  "Set KEY to VALUE in OBJECT only when KEY already exists."
  (pcase (lg-json--object-kind object)
    ('hash-table
     (let* ((missing (make-symbol "lg-json-missing"))
            (current (gethash key object missing)))
       (if (eq current missing)
           object
         (let ((copy (copy-hash-table object)))
           (puthash key value copy)
           copy))))
    ('alist
     (let ((updated nil))
       (mapcar (lambda (entry)
                 (if (and (not updated) (funcall testfn (car entry) key))
                     (progn
                       (setq updated t)
                       (cons (car entry) value))
                   entry))
               object)))
    ('plist
     (let ((rest object)
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
    (_ (error "Expected JSON object representation"))))

(defun lg-json--set-object-key-maybe (object key maybe testfn)
  "Set KEY in OBJECT according to tagged MAYBE using TESTFN."
  (unless (or (lg-just-p maybe) (lg-nothing-p maybe))
    (error "Expected tagged maybe value"))
  (pcase (lg-json--object-kind object)
    ('hash-table
     (let ((copy (copy-hash-table object)))
       (if (lg-just-p maybe)
           (puthash key (cdr maybe) copy)
         (remhash key copy))
       copy))
    ('alist
     (let ((rest object)
           (result nil)
           (updated nil)
           (removed nil))
       (dolist (entry rest)
         (if (and (not updated) (funcall testfn (car entry) key))
             (progn
               (setq updated t)
               (if (lg-just-p maybe)
                   (push (cons (car entry) (cdr maybe)) result)
                 (setq removed t)))
           (push entry result)))
       (setq result (nreverse result))
       (cond
        ((lg-just-p maybe)
         (if updated
             result
           (append result (list (cons key (cdr maybe))))))
        (removed result)
        (t result))))
    ('plist
     (let ((rest object)
           (result nil)
           (updated nil)
           (removed nil))
       (while rest
         (let ((candidate (car rest))
               (candidate-value (cadr rest)))
           (if (and (not updated) (funcall testfn candidate key))
               (progn
                 (setq updated t)
                 (if (lg-just-p maybe)
                     (setq result (append result (list candidate (cdr maybe))))
                   (setq removed t)))
             (setq result (append result (list candidate candidate-value)))))
         (setq rest (cddr rest)))
       (cond
        ((lg-just-p maybe)
         (if updated
             result
           (append result (list key (cdr maybe)))))
        (removed result)
        (t result))))
    (_ (error "Expected JSON object representation"))))

(defun lg-json--array-ref-maybe (array index)
  "Return tagged maybe of ARRAY element at INDEX."
  (if (and (integerp index) (>= index 0) (< index (length array)))
      (lg-just (if (vectorp array) (aref array index) (nth index array)))
    lg-nothing))

(defun lg-json--array-set-existing (array index value)
  "Set ARRAY element at INDEX to VALUE when INDEX is in range."
  (if (and (integerp index) (>= index 0) (< index (length array)))
      (if (vectorp array)
          (let ((copy (copy-sequence array)))
            (aset copy index value)
            copy)
        (let ((copy (copy-sequence array)))
          (setf (nth index copy) value)
          copy))
    array))

(defun lg-json-object-key (key &optional testfn)
  "Affine traversal focusing existing KEY in a JSON object."
  (lg-affine
   (lambda (object)
     (lg-json--present-object-key object key (or testfn #'equal)))
   (lambda (object new-focus)
     (lg-json--set-existing-object-key object key new-focus (or testfn #'equal)))))

(defun lg-json-object-key-at (key &optional testfn)
  "Lens focusing tagged maybe value for KEY in a JSON object."
  (lg-lens
   (lambda (object)
     (lg-json--present-object-key object key (or testfn #'equal)))
   (lambda (object maybe)
     (lg-json--set-object-key-maybe object key maybe (or testfn #'equal)))))

(defun lg-json-array-index (index)
  "Affine traversal focusing existing INDEX in a JSON array."
  (lg-affine
   (lambda (array)
     (unless (lg-json--array-p array)
       (error "Expected JSON array representation"))
     (lg-json--array-ref-maybe array index))
   (lambda (array new-focus)
     (unless (lg-json--array-p array)
       (error "Expected JSON array representation"))
     (lg-json--array-set-existing array index new-focus))))

(defun lg-json--plist->pairs (plist)
  "Convert PLIST to an alist-style list of (KEY . VALUE) pairs."
  (let ((rest plist)
        (pairs nil))
    (while rest
      (push (cons (car rest) (cadr rest)) pairs)
      (setq rest (cddr rest)))
    (nreverse pairs)))

(defun lg-json--pairs->plist (pairs)
  "Convert PAIRS in (KEY . VALUE) shape to plist form."
  (let ((result nil))
    (dolist (entry pairs result)
      (setq result (append result (list (car entry) (cdr entry)))))))

(defun lg-json--hash-keys (table)
  "Return a stable list of keys from hash TABLE for one traversal run."
  (let ((keys nil))
    (maphash (lambda (key _value) (push key keys)) table)
    (nreverse keys)))

(defconst lg-json-values
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (pcase (lg-json--object-kind source)
         ('alist
          (lg--traverse-list
           applicative
           (lambda (entry)
             (funcall fmap
                      (lambda (new-value) (cons (car entry) new-value))
                      (funcall afb (cdr entry))))
           source))
         ('plist
          (funcall fmap
                   #'lg-json--pairs->plist
                   (lg--traverse-list
                    applicative
                    (lambda (entry)
                      (funcall fmap
                               (lambda (new-value) (cons (car entry) new-value))
                               (funcall afb (cdr entry))))
                    (lg-json--plist->pairs source))))
         ('hash-table
          (let ((keys (lg-json--hash-keys source)))
            (funcall fmap
                     (lambda (new-values)
                       (let ((copy (copy-hash-table source)))
                         (cl-mapc (lambda (key value)
                                    (puthash key value copy))
                                  keys
                                  new-values)
                         copy))
                     (lg--traverse-list
                      applicative
                      (lambda (key)
                        (funcall afb (gethash key source)))
                      keys))))
         (_ (error "Expected JSON object representation"))))))
  "Traversal over values in a JSON object.")

(defconst lg-json-members
  (lg-indexed
   (lambda (iafb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (pcase (lg-json--object-kind source)
         ('alist
          (lg--traverse-list
           applicative
           (lambda (entry)
             (let ((key (car entry)))
               (funcall fmap
                        (lambda (new-value) (cons key new-value))
                        (funcall iafb key (cdr entry)))))
           source))
         ('plist
          (funcall fmap
                   #'lg-json--pairs->plist
                   (lg--traverse-list
                    applicative
                    (lambda (entry)
                      (let ((key (car entry)))
                        (funcall fmap
                                 (lambda (new-value) (cons key new-value))
                                 (funcall iafb key (cdr entry)))))
                    (lg-json--plist->pairs source))))
         ('hash-table
          (let ((keys (lg-json--hash-keys source)))
            (funcall fmap
                     (lambda (new-values)
                       (let ((copy (copy-hash-table source)))
                         (cl-mapc (lambda (key value)
                                    (puthash key value copy))
                                  keys
                                  new-values)
                         copy))
                     (lg--traverse-list
                      applicative
                      (lambda (key)
                        (funcall iafb key (gethash key source)))
                      keys))))
         (_ (error "Expected JSON object representation"))))))
  "Indexed traversal over members in a JSON object.")

(defconst lg-json-string
  (lg-prism
   (lambda (value)
     (if (stringp value) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing a JSON string scalar.")

(defconst lg-json-number
  (lg-prism
   (lambda (value)
     (if (numberp value) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing a JSON number scalar.")

(defconst lg-json-bool
  (lg-prism
   (lambda (value)
     (cond
      ((eq value t) (lg-right t))
      ((equal value lg-json-false-object) (lg-right nil))
      (t (lg-left value))))
   (lambda (value)
     (if value t lg-json-false-object)))
  "Prism focusing JSON booleans as t/nil.")

(defconst lg-json-null-prism
  (lg-prism
   (lambda (value)
     (if (equal value lg-json-null-object)
         (lg-right value)
       (lg-left value)))
   (lambda (_value) lg-json-null-object))
  "Prism focusing JSON null sentinel values.")

(defun lg-json-parse-string (json-text &optional object-type array-type null-object false-object)
  "Parse JSON-TEXT into an Elisp value.
Optional OBJECT-TYPE, ARRAY-TYPE, NULL-OBJECT and FALSE-OBJECT override defaults."
  (json-parse-string json-text
                     :object-type (or object-type lg-json-object-type)
                     :array-type (or array-type lg-json-array-type)
                     :null-object (or null-object lg-json-null-object)
                     :false-object (or false-object lg-json-false-object)))

(defun lg-json-render-string (value &optional null-object false-object)
  "Render VALUE into JSON text.
Optional NULL-OBJECT and FALSE-OBJECT override defaults."
  (json-serialize (lg-json--normalize-for-serialize value)
                  :null-object (or null-object lg-json-null-object)
                  :false-object (or false-object lg-json-false-object)))

(defun lg-json--normalize-for-serialize (value)
  "Normalize VALUE into shapes accepted by `json-serialize'."
  (cond
   ((vectorp value)
    (vconcat (mapcar #'lg-json--normalize-for-serialize (append value nil))))
   ((hash-table-p value)
    (let ((copy (copy-hash-table value)))
      (maphash (lambda (key item)
                 (puthash key (lg-json--normalize-for-serialize item) copy))
               value)
      copy))
   ((lg-json--alist-p value)
    (mapcar (lambda (entry)
              (cons (car entry)
                    (lg-json--normalize-for-serialize (cdr entry))))
            value))
   ((and (listp value)
         (zerop (% (length value) 2))
         (cl-loop for rest on value by #'cddr
                  always (symbolp (car rest))))
    (let ((rest value)
          (result nil))
      (while rest
        (setq result
              (append result
                      (list (car rest)
                            (lg-json--normalize-for-serialize (cadr rest)))))
        (setq rest (cddr rest)))
      result))
   ((listp value)
    (vconcat (mapcar #'lg-json--normalize-for-serialize value)))
   (t value)))

(defconst lg-json-text-prism
  (lg-prism
   (lambda (value)
     (if (stringp value)
         (condition-case nil
             (lg-right (lg-json-parse-string value))
           (error (lg-left value)))
       (lg-left value)))
   #'lg-json-render-string)
  "Prism between JSON text and parsed JSON values.")

(defalias 'j-object-key #'lg-json-object-key)
(defalias 'j-object-key-at #'lg-json-object-key-at)
(defalias 'j-array-index #'lg-json-array-index)
(defconst j-values lg-json-values)
(defconst j-members lg-json-members)
(defconst j-string lg-json-string)
(defconst j-number lg-json-number)
(defconst j-bool lg-json-bool)
(defconst j-null lg-json-null-prism)
(defconst j-text lg-json-text-prism)

(provide 'looking-glass-json)

;;; looking-glass-json.el ends here
