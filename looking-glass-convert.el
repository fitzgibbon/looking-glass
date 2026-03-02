;;; looking-glass-convert.el --- Conversion optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6"))
;; Keywords: lisp, extensions

;;; Commentary:

;; Conversion-focused optics for core Emacs Lisp datatypes.

;;; Code:

(require 'cl-lib)
(require 'looking-glass)

(defconst lg-list-vector-iso
  (lg-iso
   (lambda (value)
     (if (listp value)
         (vconcat value)
       (error "Expected list source for lg-list-vector-iso")))
   (lambda (value)
     (if (vectorp value)
         (append value nil)
       (error "Expected vector focus for lg-list-vector-iso"))))
  "Iso between list and vector.
Forward maps list -> vector, backward maps vector -> list.")

(defun lg--parse-number-string (value)
  "Parse VALUE into a number with full-string consumption.
Return the number on success, or nil on failure."
  (when (stringp value)
    (let ((parsed (ignore-errors (read-from-string value))))
      (when parsed
        (let ((candidate (car parsed))
              (position (cdr parsed)))
          (when (and (numberp candidate)
                     (string-match-p "\\`[[:space:]]*\\'" (substring value position)))
            candidate))))))

(defconst lg-number-string-prism
  (lg-prism
   (lambda (value)
     (let ((parsed (lg--parse-number-string value)))
       (if parsed
           (lg-right parsed)
         (lg-left value))))
   (lambda (value)
     (if (numberp value)
         (number-to-string value)
       (error "Expected number focus for lg-number-string-prism"))))
  "Prism between string and number.
Match parses string -> number; review renders number -> string.")

(defconst lg-char-string-prism
  (lg-prism
   (lambda (value)
     (if (and (stringp value)
              (= (length value) 1)
              (characterp (aref value 0)))
         (lg-right (aref value 0))
       (lg-left value)))
   (lambda (value)
     (if (characterp value)
         (char-to-string value)
       (error "Expected character focus for lg-char-string-prism"))))
  "Prism between string of length 1 and character code.")

(defconst lg-symbol-string-prism
  (lg-prism
   (lambda (value)
     (if (stringp value)
         (let ((symbol (intern-soft value)))
           (if symbol
               (lg-right symbol)
             (lg-left value)))
       (lg-left value)))
   (lambda (value)
     (if (symbolp value)
         (symbol-name value)
       (error "Expected symbol focus for lg-symbol-string-prism"))))
  "Prism between interned symbol names and strings.
Match uses `intern-soft' and fails when string is not interned.")

(defun lg--plist-even-p (plist)
  "Return non-nil when PLIST has even length."
  (zerop (% (length plist) 2)))

(defun lg--alist->plist-maybe (alist)
  "Return plist converted from ALIST, or nil when invalid.
Accepted entries are cons pairs with symbol keys and unique keys."
  (when (listp alist)
    (let ((seen (make-hash-table :test 'eq))
          (result nil)
          (ok t))
      (dolist (entry alist)
        (if (and (consp entry)
                 (symbolp (car entry))
                 (not (gethash (car entry) seen)))
            (progn
              (puthash (car entry) t seen)
              (setq result (append result (list (car entry) (cdr entry)))))
          (setq ok nil)))
      (and ok result))))

(defun lg--plist->alist-checked (plist)
  "Return alist converted from PLIST or signal on invalid data.
Accepted keys are symbols and keys must be unique."
  (unless (and (listp plist) (lg--plist-even-p plist))
    (error "Expected even-length plist"))
  (let ((seen (make-hash-table :test 'eq))
        (rest plist)
        (result nil))
    (while rest
      (let ((key (car rest))
            (value (cadr rest)))
        (unless (symbolp key)
          (error "Expected symbol key in plist"))
        (when (gethash key seen)
          (error "Duplicate key in plist: %S" key))
        (puthash key t seen)
        (setq result (append result (list (cons key value)))))
      (setq rest (cddr rest)))
    result))

(defconst lg-alist-plist-prism
  (lg-prism
   (lambda (value)
     (let ((plist (lg--alist->plist-maybe value)))
       (if (or plist (null value))
           (lg-right (or plist nil))
         (lg-left value))))
   #'lg--plist->alist-checked)
  "Prism between constrained alist and constrained plist.
Accepted keys are unique symbols.")

(provide 'looking-glass-convert)

;;; looking-glass-convert.el ends here
