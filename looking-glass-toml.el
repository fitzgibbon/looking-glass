;;; looking-glass-toml.el --- TOML optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6") (looking-glass "0.1.0"))
;; Keywords: lisp, toml, extensions

;;; Commentary:

;; TOML-oriented optics over decoded Elisp structures.

;;; Code:

(require 'cl-lib)
(require 'looking-glass)

(defun lg-toml-key (key &optional testfn)
  "Affine traversal focusing existing KEY in TOML table values."
  (lg-ix key (or testfn #'equal)))

(defun lg-toml-key-at (key &optional testfn)
  "Lens focusing tagged maybe value of KEY in TOML table values."
  (lg-at key (or testfn #'equal)))

(defun lg-toml-array-index (index)
  "Affine traversal focusing INDEX in TOML arrays."
  (lg-affine
   (lambda (source)
     (if (or (vectorp source) (listp source))
         (if (and (integerp index) (>= index 0) (< index (length source)))
             (lg-just (if (vectorp source) (aref source index) (nth index source)))
           lg-nothing)
       (error "Expected TOML array value")))
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
       (error "Expected TOML array value")))))

(defconst lg-toml-string
  (lg-prism
   (lambda (value)
     (if (stringp value) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing TOML string values.")

(defconst lg-toml-number
  (lg-prism
   (lambda (value)
     (if (numberp value) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing TOML number values.")

(defconst lg-toml-bool
  (lg-prism
   (lambda (value)
     (if (or (eq value t) (eq value nil)) (lg-right value) (lg-left value)))
   #'identity)
  "Prism focusing TOML boolean values.")

(defun lg-toml--parse-string-with-library (toml-text)
  "Parse TOML-TEXT using whichever TOML parser is available."
  (cond
   ((fboundp 'toml-parse-string)
    (funcall #'toml-parse-string toml-text))
   ((fboundp 'toml-parse)
    (funcall #'toml-parse toml-text))
   (t
    (error "No TOML parser found; install a TOML library exposing toml-parse-string or toml-parse"))))

(defconst lg-toml-text-prism
  (lg-prism
   (lambda (value)
     (if (stringp value)
         (condition-case nil
             (lg-right (lg-toml--parse-string-with-library value))
           (error (lg-left value)))
       (lg-left value)))
   (lambda (_value)
     (error "TOML rendering is not available in this package")))
  "Prism for parsing TOML text into Elisp values.")

(provide 'looking-glass-toml)

;;; looking-glass-toml.el ends here
