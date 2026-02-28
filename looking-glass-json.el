;;; looking-glass-json.el --- JSON optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (looking-glass "0.4.0"))
;; Keywords: lisp, data, json, tools
;; URL: https://github.com/fitzgibbon/looking-glass

;;; Commentary:

;; JSON-focused optics built on top of looking-glass.
;;
;; This package provides:
;; - boundary prisms for parsing/serializing JSON strings
;; - object/array optics for decoded JSON structures
;; - path composition helpers for nested updates

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'looking-glass)

(defun lg-json-string (&rest options)
  "Prism between JSON text and decoded Emacs values.

OPTIONS is a plist forwarded to `json-parse-string', commonly using
`:object-type', `:array-type', `:null-object', and `:false-object'."
  (let ((object-type (plist-get options :object-type))
        (array-type (plist-get options :array-type))
        (null-object (plist-get options :null-object))
        (false-object (plist-get options :false-object)))
    (lg-prism
     (lambda (source)
       (when (stringp source)
         (condition-case nil
             (cons t
                   (json-parse-string source
                                      :object-type object-type
                                      :array-type array-type
                                      :null-object null-object
                                      :false-object false-object))
           (error nil))))
     (lambda (value)
       (json-serialize value
                       :null-object null-object
                       :false-object false-object)))))

(defun lg-json-string-alist ()
  "Prism between JSON text and alist/list decoded structures."
  (lg-json-string :object-type 'alist :array-type 'list))

(defun lg-json-string-plist ()
  "Prism between JSON text and plist/list decoded structures."
  (lg-json-string :object-type 'plist :array-type 'list))

(defun lg-json-string-hash ()
  "Prism between JSON text and hash-table/vector decoded structures."
  (lg-json-string :object-type 'hash-table :array-type 'vector))

(defun lg-json--plist-p (value)
  "Return non-nil when VALUE looks like a plist object." 
  (and (listp value)
       (or (null value)
           (and (not (consp (car value)))
                (zerop (% (length value) 2))))))

(defun lg-json--alist-p (value)
  "Return non-nil when VALUE looks like an alist object." 
  (and (listp value)
       (or (null value)
           (consp (car value)))))

(defun lg-json--assoc-get (key alist test)
  "Return first value for KEY in ALIST under TEST."
  (cl-labels ((match-p (a b)
                (if test
                    (funcall test a b)
                  (or (equal a b)
                      (and (symbolp a) (stringp b)
                           (string= (symbol-name a) b))
                      (and (stringp a) (symbolp b)
                           (string= a (symbol-name b)))))))
  (let ((found nil)
        (value nil))
    (while (and alist (not found))
      (let ((cell (car alist)))
        (when (and (consp cell)
                   (match-p key (car cell)))
          (setq value (cdr cell))
          (setq found t)))
      (setq alist (cdr alist)))
      value)))

(defun lg-json--assoc-set (key new alist test)
  "Set first value for KEY to NEW in ALIST under TEST.

Insert at front when KEY is not present."
  (cl-labels ((match-p (a b)
                (if test
                    (funcall test a b)
                  (or (equal a b)
                      (and (symbolp a) (stringp b)
                           (string= (symbol-name a) b))
                      (and (stringp a) (symbolp b)
                           (string= a (symbol-name b)))))))
    (let ((out nil)
          (done nil))
      (dolist (cell alist)
        (if (and (not done)
                 (consp cell)
                 (match-p key (car cell)))
            (progn
              (push (cons (car cell) new) out)
              (setq done t))
          (push cell out)))
      (unless done
        (push (cons key new) out))
      (nreverse out))))

(defun lg-jkey (key &optional test)
  "Lens focusing KEY in a decoded JSON object.

Supported object representations are hash tables, alists, and plists.
For alists, TEST controls key equality (defaults to `equal')."
  (lg-lens
   (lambda (source)
     (cond
      ((hash-table-p source) (gethash key source))
      ((lg-json--alist-p source) (lg-json--assoc-get key source test))
      ((lg-json--plist-p source) (plist-get source key))
      (t (error "`lg-jkey' expects JSON object, got: %S" source))))
   (lambda (new source)
     (cond
      ((hash-table-p source)
       (let ((copy (copy-hash-table source)))
         (puthash key new copy)
         copy))
      ((lg-json--alist-p source)
       (lg-json--assoc-set key new source test))
      ((lg-json--plist-p source)
       (let ((copy (copy-sequence source)))
         (plist-put copy key new)
         copy))
      (t (error "`lg-jkey' expects JSON object, got: %S" source))))))

(defun lg-ijkey (key &optional test)
  "Indexed variant of `lg-jkey'.  Focus index is KEY."
  (lg-ilens
   (lambda (source)
     (lg-view (lg-jkey key test) source))
   (lambda (new source)
     (lg-set (lg-jkey key test) new source))
   (lambda (_source) key)))

(defun lg-jindex (index)
  "Lens focusing INDEX in a decoded JSON array (vector or list)."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-lens
   (lambda (source)
     (cond
      ((vectorp source) (aref source index))
      ((listp source) (nth index source))
      (t (error "`lg-jindex' expects JSON array, got: %S" source))))
   (lambda (new source)
     (cond
      ((vectorp source)
       (let ((copy (copy-sequence source)))
         (aset copy index new)
         copy))
      ((listp source)
       (let ((copy (copy-sequence source)))
         (setf (nth index copy) new)
         copy))
      (t (error "`lg-jindex' expects JSON array, got: %S" source))))))

(defun lg-ijindex (index)
  "Indexed variant of `lg-jindex'.  Focus index is INDEX."
  (lg-ilens
   (lambda (source)
     (lg-view (lg-jindex index) source))
   (lambda (new source)
     (lg-set (lg-jindex index) new source))
   (lambda (_source) index)))

(defun lg-jvalues ()
  "Traversal over values in decoded JSON arrays.

Supports vectors and lists."
  (lg-wander
   (lambda (step source app)
     (cond
      ((vectorp source)
       (let* ((pure (lg-applicative-pure app))
              (map2 (lg-applicative-map2 app))
              (acc (funcall pure nil))
              (i 0)
              (len (length source)))
         (while (< i len)
           (setq acc
                 (funcall map2
                           (lambda (xs y) (append xs (list y)))
                           acc
                           (funcall step (aref source i))))
           (setq i (1+ i)))
         (lg--app-map app #'vconcat acc)))
      ((listp source)
       (let* ((pure (lg-applicative-pure app))
              (map2 (lg-applicative-map2 app))
              (acc (funcall pure nil)))
          (dolist (value source acc)
            (setq acc
                  (funcall map2
                           (lambda (xs y) (append xs (list y)))
                           acc
                           (funcall step value))))))
      (t (error "`lg-jvalues' expects JSON array, got: %S" source))))))

(defun lg-ijvalues ()
  "Indexed traversal over values in decoded JSON arrays.

Indices are zero-based positions."
  (lg-iwander
   (lambda (step source app)
     (cond
      ((vectorp source)
       (let* ((pure (lg-applicative-pure app))
              (map2 (lg-applicative-map2 app))
              (acc (funcall pure nil))
              (i 0)
              (len (length source)))
         (while (< i len)
           (let ((idx i))
             (setq acc
                   (funcall map2
                            (lambda (xs idx+value)
                              (append xs (list (cdr idx+value))))
                            acc
                            (funcall step (cons idx (aref source i))))))
           (setq i (1+ i)))
         (lg--app-map app #'vconcat acc)))
      ((listp source)
       (let* ((pure (lg-applicative-pure app))
              (map2 (lg-applicative-map2 app))
              (acc (funcall pure nil))
              (index 0))
         (dolist (value source acc)
           (let ((idx index))
             (setq acc
                   (funcall map2
                            (lambda (xs idx+value)
                             (append xs (list (cdr idx+value))))
                             acc
                             (funcall step (cons idx value)))))
           (setq index (1+ index)))))
      (t (error "`lg-ijvalues' expects JSON array, got: %S" source))))))

(defun lg-jpath (&rest segments)
  "Build a composed JSON optic from SEGMENTS.

Each segment must be:
- integer: array index via `lg-jindex'
- string/symbol/keyword: object key via `lg-jkey'"
  (let ((optics
         (mapcar (lambda (segment)
                   (cond
                    ((integerp segment) (lg-jindex segment))
                    ((or (stringp segment) (symbolp segment))
                     (lg-jkey segment))
                    (t (error "Unsupported JSON path segment: %S" segment))))
                 segments)))
    (apply #'lg-compose optics)))

(defmacro lg/path-json (&rest segments)
  "Macro sugar for `lg-jpath'."
  `(lg-jpath ,@segments))

(defmacro lg^ (source optic)
  "View SOURCE through OPTIC." 
  `(lg-view ,optic ,source))

(defmacro lg= (source optic value)
  "Set VALUE through OPTIC in SOURCE." 
  `(lg-set ,optic ,value ,source))

(defmacro lg~ (source optic fn)
  "Apply FN through OPTIC in SOURCE." 
  `(lg-over ,optic ,fn ,source))

(cl-defmacro lg~i (source optic (index value) &rest body)
  "Apply indexed BODY through OPTIC in SOURCE.

INDEX and VALUE are bound per focused element."
  `(lg-iover ,optic
             (lambda (,index ,value)
               ,@body)
             ,source))

(defmacro lg-> (x &rest forms)
  "Thread X through FORMS as first argument.

This is a thin alias over `thread-first'."
  `(thread-first ,x ,@forms))

(defmacro lg->> (x &rest forms)
  "Thread X through FORMS as last argument.

This is a thin alias over `thread-last'."
  `(thread-last ,x ,@forms))

(provide 'looking-glass-json)

;;; looking-glass-json.el ends here
