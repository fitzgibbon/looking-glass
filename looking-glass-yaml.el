;;; looking-glass-yaml.el --- YAML optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6") (looking-glass "0.1.0"))
;; Keywords: lisp, yaml, extensions

;;; Commentary:

;; YAML-oriented optics over decoded Elisp structures.

;;; Code:

(require 'looking-glass)

(defconst lg-yaml-null 'lg-yaml-null
  "Default sentinel used to represent YAML null values.")

(defun lg-yaml-key (key &optional testfn)
  "Affine traversal focusing existing KEY in YAML mapping values."
  (lg-ix key (or testfn #'equal)))

(defun lg-yaml-key-at (key &optional testfn)
  "Lens focusing tagged maybe value of KEY in YAML mapping values."
  (lg-at key (or testfn #'equal)))

(defun lg-yaml-index (index)
  "Affine traversal focusing INDEX in YAML sequence values."
  (lg-affine
   (lambda (source)
     (if (or (vectorp source) (listp source))
         (if (and (integerp index) (>= index 0) (< index (length source)))
             (lg-just (if (vectorp source) (aref source index) (nth index source)))
           lg-nothing)
       (error "Expected YAML sequence value")))
   (lambda (source new-focus)
     (if (or (vectorp source) (listp source))
         (if (and (integerp index) (>= index 0) (< index (length source)))
             (if (vectorp source)
                 (let ((copy (copy-sequence source)))
                   (aset copy index new-focus)
                   copy)
               (let ((copy (copy-sequence source)))
                 (setf (nth index copy) new-focus)
                 copy))
           source)
       (error "Expected YAML sequence value")))))

(defconst lg-yaml-string
  (lg-prism
   (lambda (value)
     (if (stringp value) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing YAML string scalar values.")

(defconst lg-yaml-number
  (lg-prism
   (lambda (value)
     (if (numberp value) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing YAML number scalar values.")

(defconst lg-yaml-bool
  (lg-prism
   (lambda (value)
     (if (or (eq value t) (eq value nil)) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing YAML boolean scalar values.")

(defconst lg-yaml-null-prism
  (lg-prism
   (lambda (value)
     (if (eq value lg-yaml-null)
         (lg-right value)
       (lg-left value)))
   (lambda (_value) lg-yaml-null))
  "Prism focusing YAML null sentinel values.")

(provide 'looking-glass-yaml)

;;; looking-glass-yaml.el ends here
