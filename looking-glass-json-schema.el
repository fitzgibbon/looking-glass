;;; looking-glass-json-schema.el --- Compile JSON Schema conversions -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (looking-glass "0.1.0"))
;; URL: https://github.com/fitzgibbon/looking-glass
;; Keywords: lisp, extensions, data

;;; Commentary:

;; Compile a JSON Schema into a pure, recursive conversion optic.  The optic
;; converts a complete native JSON value into its semantic representation and
;; reviews it back to JSON-native data.  JSON parsing and serialization remain
;; the responsibility of looking-glass or Emacs JSON primitives.

;;; Code:

(require 'cl-lib)
(require 'parse-time)
(require 'looking-glass)

(cl-defstruct lg-json-schema-date
  "Semantic calendar date produced by the `date' format converter."
  year
  month
  day)

(cl-defstruct (lg-json-schema-converter-set
               (:constructor lg-json-schema--make-converter-set))
  "Compiled root and named-definition conversion optics."
  root
  definitions)

(cl-defstruct (lg-json-schema--context
               (:constructor lg-json-schema--make-context))
  root
  schema-object-kind
  object-type
  array-type
  null-object
  false-object
  formats
  cache)

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

(defun lg-json-schema--decode-pointer-token (token)
  "Decode one JSON Pointer TOKEN."
  (replace-regexp-in-string
   "~1" "/"
   (replace-regexp-in-string "~0" "~" token t t)
   t t))

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
               current (lg-json-schema--decode-pointer-token token)))
        (when (eq current (lg-json-schema--missing))
          (error "Unresolved JSON Schema reference: %s" reference)))
      current)))

(defun lg-json-schema--valid-date-p (year month day)
  "Return non-nil when YEAR, MONTH, and DAY form a calendar date."
  (condition-case nil
      (let ((decoded
             (decode-time (encode-time 0 0 12 day month year t) t)))
        (and (= year (decoded-time-year decoded))
             (= month (decoded-time-month decoded))
             (= day (decoded-time-day decoded))))
    (error nil)))

(defun lg-json-schema--date-forward (value)
  "Convert valid full-date string VALUE to a semantic date."
  (unless (and (stringp value)
               (string-match
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
                value))
    (error "Expected JSON Schema full-date string, got %S" value))
  (let ((year (string-to-number (match-string 1 value)))
        (month (string-to-number (match-string 2 value)))
        (day (string-to-number (match-string 3 value))))
    (unless (lg-json-schema--valid-date-p year month day)
      (error "Invalid JSON Schema full-date string: %s" value))
    (make-lg-json-schema-date :year year :month month :day day)))

(defun lg-json-schema--date-backward (value)
  "Convert semantic date VALUE to canonical full-date text."
  (unless (lg-json-schema-date-p value)
    (error "Expected lg-json-schema-date, got %S" value))
  (let ((year (lg-json-schema-date-year value))
        (month (lg-json-schema-date-month value))
        (day (lg-json-schema-date-day value)))
    (unless (and (integerp year) (<= 0 year 9999)
                 (integerp month) (<= 1 month 12)
                 (integerp day) (<= 1 day 31))
      (error "Invalid semantic date: %S" value))
    (lg-json-schema--date-forward (format "%04d-%02d-%02d" year month day))
    (format "%04d-%02d-%02d" year month day)))

(defconst lg-json-schema-date-iso
  (lg-iso #'lg-json-schema--date-forward #'lg-json-schema--date-backward)
  "Iso from valid JSON Schema full-date text to semantic dates.")

(defun lg-json-schema--date-time-forward (value)
  "Convert valid RFC 3339 date-time VALUE to an Emacs time value."
  (unless (and
           (stringp value)
           (string-match
            (concat
             "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-"
             "\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\):"
             "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
             "\\(?:\\.\\([0-9]+\\)\\)?"
             "\\(Z\\|[+-]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)\\'")
            value))
    (error "Expected JSON Schema RFC 3339 date-time, got %S" value))
  (let* ((year (string-to-number (match-string 1 value)))
         (month (string-to-number (match-string 2 value)))
         (day (string-to-number (match-string 3 value)))
         (hour (string-to-number (match-string 4 value)))
         (minute (string-to-number (match-string 5 value)))
         (second (string-to-number (match-string 6 value)))
         (fraction (match-string 7 value))
         (zone (match-string 8 value))
         (zone-hour (match-string 9 value))
         (zone-minute (match-string 10 value)))
    (unless (and (lg-json-schema--valid-date-p year month day)
                 (<= 0 hour 23)
                 (<= 0 minute 59)
                 (or (<= 0 second 59)
                     (and (= second 60) (= hour 23) (= minute 59)))
                 (or (string= zone "Z")
                     (and (<= 0 (string-to-number zone-hour) 23)
                          (<= 0 (string-to-number zone-minute) 59))))
      (error "Invalid JSON Schema RFC 3339 date-time: %s" value))
    (let* ((whole (format "%04d-%02d-%02dT%02d:%02d:%02d"
                          year month day hour minute second))
           (base (condition-case nil
                     (parse-iso8601-time-string (concat whole zone))
                   (error nil))))
      (unless base
        (error "Invalid JSON Schema RFC 3339 date-time: %s" value))
      (if fraction
          (let* ((digits (substring (concat fraction "000000000000") 0 12))
                 (microseconds (string-to-number (substring digits 0 6)))
                 (picoseconds (string-to-number (substring digits 6 12))))
            (time-add base (list 0 0 microseconds picoseconds)))
        base))))

(defun lg-json-schema--date-time-backward (value)
  "Convert Emacs time VALUE to canonical UTC RFC 3339 text."
  (let* ((frequency 1000000000000)
         (converted
          (condition-case nil
              (time-convert value frequency)
            (error (error "Expected Emacs time value, got %S" value))))
         (ticks (car converted))
         (remainder (mod ticks frequency))
         (whole-seconds (/ (- ticks remainder) frequency))
         (whole-time (seconds-to-time whole-seconds))
         (prefix (format-time-string "%Y-%m-%dT%H:%M:%S" whole-time t))
         (fraction
          (replace-regexp-in-string
           "0+\\'" "" (format "%012d" remainder))))
    (unless (string-match-p "\\`[0-9]\\{4\\}-" prefix)
      (error "Time is outside the RFC 3339 four-digit year range: %S" value))
    (concat prefix
            (if (string-empty-p fraction) "" (concat "." fraction))
            "Z")))

(defconst lg-json-schema-date-time-iso
  (lg-iso #'lg-json-schema--date-time-forward
          #'lg-json-schema--date-time-backward)
  "Canonicalizing iso from RFC 3339 text to Emacs time values.")

(defconst lg-json-schema-default-formats
  `(("date" . ,lg-json-schema-date-iso)
    ("date-time" . ,lg-json-schema-date-time-iso))
  "Default semantic conversion optics keyed by JSON Schema format.")

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

(defun lg-json-schema--array-tail-optic (array-type start)
  "Return an element traversal from START onward for ARRAY-TYPE."
  (if (zerop start)
      (lg-json-schema--array-optic array-type)
    (lg-unindexed
     (lg-compose-indexed
      (pcase array-type
        ('array lg-indexed-vector)
        ('list lg-indexed-list)
        (_ (error "Unsupported JSON array representation: %S" array-type)))
      (lg-indices (lambda (index) (>= index start)))))))

(defun lg-json-schema--sequence (value)
  "Return schema array VALUE as a list."
  (cond
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (error "Expected a JSON Schema array, got %S" value))))

(defun lg-json-schema--apply-converter (converter direction value)
  "Apply CONVERTER in DIRECTION to VALUE."
  (if (eq direction 'forward)
      (lg-view converter value)
    (lg-review converter value)))

(defun lg-json-schema--lift (optic converter)
  "Lift subtree CONVERTER through OPTIC into a whole-value iso."
  (if (eq converter lg-id)
      lg-id
    (lg-iso
     (lambda (source)
       (lg-over optic
                (lambda (value)
                  (lg-json-schema--apply-converter
                   converter 'forward value))
                source))
     (lambda (source)
       (lg-over optic
                (lambda (value)
                  (lg-json-schema--apply-converter
                   converter 'backward value))
                source)))))

(defun lg-json-schema--compose-converters (converters)
  "Compose CONVERTERS after removing identity optics."
  (let ((active (delq lg-id (copy-sequence converters))))
    (if active (apply #'lg-compose active) lg-id)))

(defun lg-json-schema--lazy-converter (cell)
  "Return a converter delegating to the optic stored in CELL."
  (lg-iso
   (lambda (value)
     (unless (car cell) (error "Recursive JSON Schema converter is not ready"))
     (lg-view (car cell) value))
   (lambda (value)
     (unless (car cell) (error "Recursive JSON Schema converter is not ready"))
     (lg-review (car cell) value))))

(defun lg-json-schema--format-converter (schema context)
  "Return the annotated format converter for SCHEMA in CONTEXT."
  (let ((format (lg-json-schema--member schema "format")))
    (if (eq format (lg-json-schema--missing))
        lg-id
      (unless (stringp format)
        (error "JSON Schema format must be a string: %S" format))
      (or (cdr (assoc format (lg-json-schema--context-formats context)))
          lg-id))))

(defun lg-json-schema--object-converter (schema context)
  "Compile object property conversions from SCHEMA in CONTEXT."
  (let ((properties (lg-json-schema--member schema "properties")))
    (if (eq properties (lg-json-schema--missing))
        lg-id
      (lg-json-schema--compose-converters
       (mapcar
        (lambda (property)
          (lg-json-schema--lift
           (lg-json-schema--property-optic
            (car property) (lg-json-schema--context-object-type context))
           (lg-json-schema--compile-node (cdr property) context)))
        (lg-json-schema--object-members properties))))))

(defun lg-json-schema--array-converter (schema context)
  "Compile array item conversions from SCHEMA in CONTEXT."
  (let ((prefix-items (lg-json-schema--member schema "prefixItems"))
        (items (lg-json-schema--member schema "items"))
        (prefix-count 0)
        converters)
    (unless (eq prefix-items (lg-json-schema--missing))
      (let ((children (lg-json-schema--sequence prefix-items)))
        (setq prefix-count (length children))
        (cl-loop for child in children
                 for index from 0
                 do (push (lg-json-schema--lift
                           (lg-nth index)
                           (lg-json-schema--compile-node child context))
                          converters))))
    (unless (eq items (lg-json-schema--missing))
      (if (or (vectorp items)
              (and (listp items)
                   (not (lg-json-schema--object-of-kind-p
                         items
                         (lg-json-schema--context-schema-object-kind context)))))
          (cl-loop for child in (lg-json-schema--sequence items)
                   for index from 0
                   do (push (lg-json-schema--lift
                             (lg-nth index)
                             (lg-json-schema--compile-node child context))
                            converters))
        (push (lg-json-schema--lift
               (lg-json-schema--array-tail-optic
                (lg-json-schema--context-array-type context) prefix-count)
               (lg-json-schema--compile-node items context))
              converters)))
    (lg-json-schema--compose-converters (nreverse converters))))

(defun lg-json-schema--data-member (source name context)
  "Return NAME from data SOURCE according to CONTEXT."
  (lg-ix-get
   source
   (pcase (lg-json-schema--context-object-type context)
     ('hash-table name)
     ('alist (intern name))
     ('plist (intern (concat ":" name))))))

(defun lg-json-schema--const-predicate (schema context)
  "Return a discriminator predicate for SCHEMA in CONTEXT, or nil."
  (let ((properties (lg-json-schema--member schema "properties"))
        result)
    (unless (eq properties (lg-json-schema--missing))
      (dolist (property (lg-json-schema--object-members properties))
        (let ((constant (lg-json-schema--member (cdr property) "const")))
          (unless (or result (eq constant (lg-json-schema--missing)))
            (let ((name (car property)))
              (setq result
                    (lambda (source)
                      (let ((value
                             (lg-json-schema--data-member source name context)))
                        (and (lg-just-p value)
                             (equal (cdr value) constant))))))))))
    result))

(defun lg-json-schema--type-predicate (schema context)
  "Return a basic JSON type predicate for SCHEMA in CONTEXT, or nil."
  (let ((type (lg-json-schema--member schema "type")))
    (when (stringp type)
      (pcase type
        ("object"
         (lambda (value)
           (pcase (lg-json-schema--context-object-type context)
             ('hash-table (hash-table-p value))
             ('alist (lg-json-schema--alist-p value))
             ('plist (lg-json-schema--plist-p value)))))
        ("array"
         (if (eq (lg-json-schema--context-array-type context) 'array)
             #'vectorp #'listp))
        ("string" #'stringp)
        ("number" #'numberp)
        ("integer"
         (lambda (value)
           (and (numberp value) (= value (truncate value)))))
        ("boolean"
         (lambda (value)
           (or (eq value t) (eq value lg-true)
               (equal value (lg-json-schema--context-false-object context))
               (eq value lg-false))))
        ("null"
         (lambda (value)
           (equal value (lg-json-schema--context-null-object context))))
        (_ nil)))))

(defun lg-json-schema--branch-predicate (schema context)
  "Return a practical alternative predicate for SCHEMA in CONTEXT."
  (let* ((reference (lg-json-schema--member schema "$ref"))
         (resolved
          (if (eq reference (lg-json-schema--missing))
              schema
            (lg-json-schema--resolve-ref
             (lg-json-schema--context-root context) reference))))
    (or (lg-json-schema--const-predicate resolved context)
        (lg-json-schema--type-predicate resolved context)
        (lambda (_value) t))))

(defun lg-json-schema--union-converter (schema keyword context)
  "Compile KEYWORD alternatives from SCHEMA in CONTEXT."
  (let ((alternatives (lg-json-schema--member schema keyword)))
    (if (eq alternatives (lg-json-schema--missing))
        lg-id
      (let ((branches
             (mapcar
              (lambda (branch)
                (cons (lg-json-schema--branch-predicate branch context)
                      (lg-json-schema--compile-node branch context)))
              (lg-json-schema--sequence alternatives))))
        (lg-iso
         (lambda (value)
           (let ((branch (cl-find-if
                          (lambda (entry) (funcall (car entry) value))
                          branches)))
             (unless branch
               (error "No %s alternative matches value %S" keyword value))
             (lg-view (cdr branch) value)))
         (lambda (value)
           (let ((branch (cl-find-if
                          (lambda (entry) (funcall (car entry) value))
                          branches)))
             (unless branch
               (error "No %s alternative matches semantic value %S"
                      keyword value))
             (lg-review (cdr branch) value))))))))

(defun lg-json-schema--all-of-converter (schema context)
  "Compile allOf conversions from SCHEMA in CONTEXT."
  (let ((alternatives (lg-json-schema--member schema "allOf")))
    (if (eq alternatives (lg-json-schema--missing))
        lg-id
      (lg-json-schema--compose-converters
       (mapcar (lambda (branch)
                 (lg-json-schema--compile-node branch context))
               (lg-json-schema--sequence alternatives))))))

(defun lg-json-schema--compile-node-body (schema context)
  "Compile the converter body for SCHEMA in CONTEXT."
  (if (or (null schema) (eq schema t) (symbolp schema))
      lg-id
    (let* ((reference (lg-json-schema--member schema "$ref"))
           (reference-converter
            (if (eq reference (lg-json-schema--missing))
                lg-id
              (unless (stringp reference)
                (error "JSON Schema $ref must be a string: %S" reference))
              (lg-json-schema--compile-node
               (lg-json-schema--resolve-ref
                (lg-json-schema--context-root context) reference)
               context))))
      (lg-json-schema--compose-converters
       (list reference-converter
             (lg-json-schema--all-of-converter schema context)
             (lg-json-schema--object-converter schema context)
             (lg-json-schema--array-converter schema context)
             (lg-json-schema--union-converter schema "oneOf" context)
             (lg-json-schema--union-converter schema "anyOf" context)
             (lg-json-schema--format-converter schema context))))))

(defun lg-json-schema--compile-node (schema context)
  "Compile SCHEMA node to a recursive conversion optic in CONTEXT."
  (if (or (null schema) (eq schema t) (symbolp schema))
      lg-id
    (let* ((cache (lg-json-schema--context-cache context))
           (cached (gethash schema cache)))
      (or cached
          (let* ((cell (list nil))
                 (lazy (lg-json-schema--lazy-converter cell)))
            (puthash schema lazy cache)
            (setcar cell (lg-json-schema--compile-node-body schema context))
            lazy)))))

(cl-defun lg-json-schema-compile
    (schema &key (object-type 'hash-table) (array-type 'array)
            null-object (false-object lg-false) formats)
  "Compile JSON SCHEMA into recursive semantic conversion optics.
SCHEMA may be raw JSON text or a native hash table, alist, or plist.
OBJECT-TYPE and ARRAY-TYPE describe the parsed document representation.
NULL-OBJECT and FALSE-OBJECT identify parser sentinels for union dispatch.
FORMATS is an alist from format strings to conversion optics; its entries
override `lg-json-schema-default-formats'.

The returned `lg-json-schema-converter-set' contains a root converter and
reusable converters for entries in `$defs'.  Converters assume schema-valid
input.  Forward conversion uses `lg-view'; backward conversion uses
`lg-review' and emits canonical representations chosen by format optics."
  (unless (memq object-type '(hash-table alist plist))
    (error "Unsupported JSON object representation: %S" object-type))
  (unless (memq array-type '(array list))
    (error "Unsupported JSON array representation: %S" array-type))
  (let* ((root (lg-json-schema--parse schema))
         (kind (lg-json-schema--object-kind root))
         (context
          (lg-json-schema--make-context
           :root root
           :schema-object-kind kind
           :object-type object-type
           :array-type array-type
           :null-object null-object
           :false-object false-object
           :formats (append formats lg-json-schema-default-formats)
           :cache (make-hash-table :test #'eq))))
    (unless (or kind (eq root t) (symbolp root))
      (error "Expected a JSON Schema object or boolean, got %S" root))
    (let* ((root-converter (lg-json-schema--compile-node root context))
           (definitions (lg-json-schema--member root "$defs"))
           (named
            (unless (eq definitions (lg-json-schema--missing))
              (mapcar
               (lambda (entry)
                 (cons (car entry)
                       (lg-json-schema--compile-node (cdr entry) context)))
               (lg-json-schema--object-members definitions)))))
      (lg-json-schema--make-converter-set
       :root root-converter
       :definitions named))))

(defun lg-json-schema-converter (converter-set &optional definition)
  "Return CONVERTER-SET root converter or named DEFINITION converter."
  (if definition
      (let ((entry (assoc definition
                          (lg-json-schema-converter-set-definitions
                           converter-set))))
        (unless entry
          (error "No compiled JSON Schema definition named %S" definition))
        (cdr entry))
    (lg-json-schema-converter-set-root converter-set)))

(defun lg-json-schema-definitions (converter-set)
  "Return names of reusable `$defs' converters in CONVERTER-SET."
  (mapcar #'car (lg-json-schema-converter-set-definitions converter-set)))

(provide 'looking-glass-json-schema)

;;; looking-glass-json-schema.el ends here
