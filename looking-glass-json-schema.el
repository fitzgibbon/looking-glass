;;; looking-glass-json-schema.el --- Generate optics from JSON Schema -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (looking-glass "0.1.0"))
;; URL: https://github.com/fitzgibbon/looking-glass
;; Keywords: lisp, extensions, data

;;; Commentary:

;; Generate addressable optics for the instance locations described by a
;; JSON Schema.  Every generated location has an optic for parsed native
;; values and one for raw JSON text.

;;; Code:

(require 'cl-lib)
(require 'looking-glass)

(cl-defstruct (lg-json-schema-entry
               (:constructor lg-json-schema--make-entry))
  "Optics generated for one JSON Pointer instance location."
  pointer
  parsed-optic
  text-optic)

(cl-defstruct (lg-json-schema-optic-set
               (:constructor lg-json-schema--make-optic-set))
  "Collection of optics generated from a JSON Schema."
  entries)

(defconst lg-json-schema--missing-marker (make-symbol "missing")
  "Private marker used for a missing schema member.")

(defun lg-json-schema--missing ()
  "Return the private marker used for a missing schema member."
  lg-json-schema--missing-marker)

(defun lg-json-schema--key-name (key)
  "Convert native JSON object KEY to its JSON member name."
  (cond
   ((stringp key) key)
   ((keywordp key) (substring (symbol-name key) 1))
   ((symbolp key) (symbol-name key))
   (t (error "Unsupported JSON Schema object key: %S" key))))

(defun lg-json-schema--alist-p (value)
  "Return non-nil when VALUE is a non-empty JSON object alist."
  (and (consp value)
       (cl-every (lambda (entry)
                   (and (consp entry)
                        (or (stringp (car entry)) (symbolp (car entry)))))
                 value)))

(defun lg-json-schema--plist-p (value)
  "Return non-nil when VALUE is a plist-like JSON object."
  (and (listp value)
       (zerop (% (length value) 2))
       (cl-loop for (key _) on value by #'cddr
                always (or (stringp key) (symbolp key)))))

(defun lg-json-schema--object-kind (object)
  "Return the native object representation used by OBJECT."
  (cond
   ((hash-table-p object) 'hash-table)
   ((lg-json-schema--alist-p object) 'alist)
   ((lg-json-schema--plist-p object) 'plist)
   (t nil)))

(defun lg-json-schema--object-of-kind-p (value kind)
  "Return non-nil when VALUE is a native JSON object of KIND."
  (pcase kind
    ('hash-table (hash-table-p value))
    ('alist (lg-json-schema--alist-p value))
    ('plist (lg-json-schema--plist-p value))
    (_ nil)))

(defun lg-json-schema--member (object name)
  "Return NAME from native JSON OBJECT, or a private missing marker."
  (let ((missing (lg-json-schema--missing)))
    (cond
     ((hash-table-p object)
      (let ((value (gethash name object missing)))
        (if (eq value missing)
            (let ((symbol (intern-soft name))
                  (keyword (intern-soft (concat ":" name))))
              (cond
               ((and symbol (not (eq (gethash symbol object missing) missing)))
                (gethash symbol object))
               ((and keyword (not (eq (gethash keyword object missing) missing)))
                (gethash keyword object))
               (t missing)))
          value)))
     ((lg-json-schema--alist-p object)
      (let ((entry (cl-find-if
                    (lambda (item)
                      (equal (lg-json-schema--key-name (car item)) name))
                    object)))
        (if entry (cdr entry) missing)))
     ((lg-json-schema--plist-p object)
      (let ((rest object)
            (result missing))
        (while rest
          (when (equal (lg-json-schema--key-name (car rest)) name)
            (setq result (cadr rest)
                  rest nil))
          (when rest (setq rest (cddr rest))))
        result))
     (t missing))))

(defun lg-json-schema--object-members (object)
  "Return native JSON OBJECT members as string-keyed cons cells."
  (cond
   ((hash-table-p object)
    (let (members)
      (maphash (lambda (key value)
                 (push (cons (lg-json-schema--key-name key) value) members))
               object)
      (nreverse members)))
   ((lg-json-schema--alist-p object)
    (mapcar (lambda (entry)
              (cons (lg-json-schema--key-name (car entry)) (cdr entry)))
            object))
   ((lg-json-schema--plist-p object)
    (cl-loop for (key value) on object by #'cddr
             collect (cons (lg-json-schema--key-name key) value)))
   ((null object) nil)
   (t (error "Expected a JSON Schema object, got %S" object))))

(defun lg-json-schema--parse (schema)
  "Parse raw JSON SCHEMA or return its native representation."
  (if (stringp schema)
      (progn
        (unless (json-available-p)
          (error "Native JSON support is required for JSON Schema text"))
        (condition-case err
            (json-parse-string schema
                               :object-type 'hash-table
                               :array-type 'array
                               :null-object nil
                               :false-object lg-false)
          (error (error "Invalid JSON Schema text: %s"
                        (error-message-string err)))))
    schema))

(defun lg-json-schema--pointer-token (name)
  "Encode JSON member NAME as one JSON Pointer token."
  (replace-regexp-in-string
   "/" "~1"
   (replace-regexp-in-string "~" "~0" name t t)
   t t))

(defun lg-json-schema--decode-token (token)
  "Decode one JSON Pointer TOKEN."
  (replace-regexp-in-string
   "~1" "/"
   (replace-regexp-in-string "~0" "~" token t t)
   t t))

(defun lg-json-schema--pointer-child (pointer name)
  "Append instance member NAME to JSON POINTER."
  (concat pointer "/" (lg-json-schema--pointer-token name)))

(defun lg-json-schema--property-optic (name object-type)
  "Return an existing-property optic for NAME and OBJECT-TYPE."
  (pcase object-type
    ('hash-table (lg-ix name))
    ('alist (lg-alist-key (intern name)))
    ('plist (lg-plist-key (intern (concat ":" name))))
    (_ (error "Unsupported JSON object representation: %S" object-type))))

(defun lg-json-schema--array-optic (array-type)
  "Return an element traversal for ARRAY-TYPE."
  (pcase array-type
    ('array lg-vector)
    ('list lg-list)
    (_ (error "Unsupported JSON array representation: %S" array-type))))

(defun lg-json-schema--sequence (value)
  "Return schema array VALUE as a list."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (error "Expected a JSON Schema array, got %S" value))))

(defun lg-json-schema--resolve-ref (root reference)
  "Resolve local JSON Schema REFERENCE against ROOT."
  (unless (string-prefix-p "#" reference)
    (error "Only local JSON Schema references are supported: %s" reference))
  (if (string= reference "#")
      root
    (unless (string-prefix-p "#/" reference)
      (error "Unsupported local JSON Schema reference: %s" reference))
    (let ((current root))
      (dolist (token (split-string (substring reference 2) "/"))
        (setq current
              (lg-json-schema--member
               current (lg-json-schema--decode-token token)))
        (when (eq current (lg-json-schema--missing))
          (error "Unresolved JSON Schema reference: %s" reference)))
      current)))

(defun lg-json-schema--walk (schema root pointer optic object-type array-type
                                     parser entries active)
  "Collect generated entries by walking SCHEMA from POINTER and OPTIC.
ROOT resolves references, PARSER builds text optics, ENTRIES deduplicates
locations, and ACTIVE tracks references during recursive expansion."
  (unless (gethash pointer entries)
    (puthash pointer
             (lg-json-schema--make-entry
              :pointer pointer
              :parsed-optic optic
              :text-optic (lg-compose parser optic))
             entries))
  (unless (or (eq schema t)
              (eq schema lg-false)
              (null schema))
    (let ((missing (lg-json-schema--missing)))
      (let ((reference (lg-json-schema--member schema "$ref")))
        (unless (eq reference missing)
          (unless (stringp reference)
            (error "JSON Schema $ref must be a string: %S" reference))
          (unless (member reference active)
            (lg-json-schema--walk
             (lg-json-schema--resolve-ref root reference)
             root pointer optic object-type array-type parser entries
             (cons reference active)))))
      (dolist (combiner '("allOf" "anyOf" "oneOf"))
        (let ((branches (lg-json-schema--member schema combiner)))
          (unless (eq branches missing)
            (dolist (branch (lg-json-schema--sequence branches))
              (lg-json-schema--walk
               branch root pointer optic object-type array-type
               parser entries active)))))
      (let ((properties (lg-json-schema--member schema "properties")))
        (unless (eq properties missing)
          (dolist (property (lg-json-schema--object-members properties))
            (let* ((name (car property))
                   (child-pointer (lg-json-schema--pointer-child pointer name))
                   (child-optic
                    (lg-compose optic
                                (lg-json-schema--property-optic name object-type))))
              (lg-json-schema--walk
               (cdr property) root child-pointer child-optic
               object-type array-type parser entries active)))))
      (let ((prefix-items (lg-json-schema--member schema "prefixItems")))
        (unless (eq prefix-items missing)
          (cl-loop for child in (lg-json-schema--sequence prefix-items)
                   for index from 0
                   do (lg-json-schema--walk
                       child root
                       (lg-json-schema--pointer-child pointer
                                                      (number-to-string index))
                       (lg-compose optic (lg-nth index))
                       object-type array-type parser entries active))))
      (let ((items (lg-json-schema--member schema "items")))
        (unless (eq items missing)
          (if (or (vectorp items)
                  (and (listp items)
                       (not (lg-json-schema--object-of-kind-p
                             items (lg-json-schema--object-kind root)))))
              (cl-loop for child in (lg-json-schema--sequence items)
                       for index from 0
                       do (lg-json-schema--walk
                           child root
                           (lg-json-schema--pointer-child
                            pointer (number-to-string index))
                           (lg-compose optic (lg-nth index))
                           object-type array-type parser entries active))
            (lg-json-schema--walk
             items root (lg-json-schema--pointer-child pointer "*")
             (lg-compose optic (lg-json-schema--array-optic array-type))
             object-type array-type parser entries active)))))))

(cl-defun lg-json-schema-generate
    (schema &key (object-type 'hash-table) (array-type 'array)
            null-object (false-object lg-false))
  "Generate an optic set from JSON SCHEMA.
SCHEMA may be raw JSON text or a native hash table, alist, or plist.
OBJECT-TYPE, ARRAY-TYPE, NULL-OBJECT, and FALSE-OBJECT describe instance
values and have the same constraints as `lg-json-parse-with'.

The set contains the empty root pointer plus paths discovered through
`properties', `items', `prefixItems', `allOf', `anyOf', `oneOf', and
local `$ref'.  Homogeneous array elements use a `*' pointer token.
Recursive references are expanded once at each encountered boundary."
  (let* ((root (lg-json-schema--parse schema))
         (parser (lg-json-parse-with object-type array-type
                                    null-object false-object))
         (entries (make-hash-table :test #'equal)))
    (unless (or (hash-table-p root)
                (lg-json-schema--alist-p root)
                (lg-json-schema--plist-p root)
                (eq root t)
                (and (symbolp root) (not (eq root t))))
      (error "Expected a JSON Schema object or boolean, got %S" root))
    (lg-json-schema--walk root root "" lg-id object-type array-type
                          parser entries nil)
    (lg-json-schema--make-optic-set
     :entries
     (sort (let (result)
             (maphash (lambda (_pointer entry) (push entry result)) entries)
             result)
           (lambda (left right)
             (string< (lg-json-schema-entry-pointer left)
                      (lg-json-schema-entry-pointer right)))))))

(defun lg-json-schema-paths (optic-set)
  "Return sorted JSON Pointer paths present in OPTIC-SET."
  (mapcar #'lg-json-schema-entry-pointer
          (lg-json-schema-optic-set-entries optic-set)))

(defun lg-json-schema-optic (optic-set pointer &optional input)
  "Return OPTIC-SET optic at JSON POINTER.
INPUT defaults to `parsed'; pass `text' for an optic over raw JSON text.
Signal when POINTER or INPUT is unknown."
  (let ((entry (cl-find pointer
                        (lg-json-schema-optic-set-entries optic-set)
                        :key #'lg-json-schema-entry-pointer
                        :test #'equal)))
    (unless entry
      (error "No generated JSON Schema optic for pointer %S" pointer))
    (pcase (or input 'parsed)
      ('parsed (lg-json-schema-entry-parsed-optic entry))
      ('text (lg-json-schema-entry-text-optic entry))
      (_ (error "JSON Schema optic input must be `parsed' or `text': %S"
                input)))))

(provide 'looking-glass-json-schema)

;;; looking-glass-json-schema.el ends here
