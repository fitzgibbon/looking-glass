;;; looking-glass-json-schema.el --- Compile JSON Schema conversions -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.3.0
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
(require 'mail-extr)
(require 'parse-time)
(require 'puny)
(require 'url-parse)
(require 'looking-glass)

(cl-defstruct lg-json-schema-date
  "Semantic calendar date produced by the `date' format converter."
  year
  month
  day)

(cl-defstruct lg-json-schema-email
  "Semantic email address split into local and domain components."
  local
  domain)

(cl-defstruct lg-json-schema-regex
  "JSON Schema regex with its source and translated Emacs regexp."
  source
  emacs-regexp)

(cl-defstruct lg-json-schema-pointer
  "Semantic JSON Pointer represented by decoded member tokens."
  tokens)

(cl-defstruct lg-json-schema-relative-pointer
  "Semantic relative JSON Pointer."
  up
  query-index-p
  tokens)

(cl-defstruct lg-json-schema-uri-template
  "URI template source and the variable names it references."
  source
  variables)

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

(defun lg-json-schema--zone-text (zone)
  "Return canonical RFC 3339 offset text for ZONE seconds."
  (unless (and (integerp zone) (zerop (% zone 60))
               (<= (abs zone) (+ (* 23 3600) (* 59 60))))
    (error "Invalid RFC 3339 timezone offset: %S" zone))
  (if (zerop zone)
      "Z"
    (let* ((sign (if (< zone 0) "-" "+"))
           (absolute (abs zone)))
      (format "%s%02d:%02d" sign (/ absolute 3600) (% (/ absolute 60) 60)))))

(defun lg-json-schema--time-forward (value)
  "Convert RFC 3339 full-time VALUE to an Emacs decoded-time list."
  (unless (and
           (stringp value)
           (string-match
            (concat
             "\\`\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\):"
             "\\([0-9]\\{2\\}\\)\\(?:\\.\\([0-9]+\\)\\)?"
             "\\(Z\\|[+-]\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)\\'")
            value))
    (error "Expected JSON Schema RFC 3339 full-time, got %S" value))
  (let* ((hour (string-to-number (match-string 1 value)))
         (minute (string-to-number (match-string 2 value)))
         (whole-second (string-to-number (match-string 3 value)))
         (fraction (match-string 4 value))
         (zone-text (match-string 5 value))
         (zone-hour (match-string 6 value))
         (zone-minute (match-string 7 value))
         (zone
          (if (string= zone-text "Z")
              0
            (* (if (string-prefix-p "-" zone-text) -1 1)
               (+ (* 3600 (string-to-number zone-hour))
                  (* 60 (string-to-number zone-minute))))))
         (second
          (+ whole-second
             (if fraction
                 (string-to-number (concat "0." fraction))
               0))))
    (unless (and (<= 0 hour 23)
                 (<= 0 minute 59)
                 (or (< second 60)
                     (and (< second 61) (= hour 23) (= minute 59)))
                 (or (string= zone-text "Z")
                     (and (<= (string-to-number zone-hour) 23)
                          (<= (string-to-number zone-minute) 59))))
      (error "Invalid JSON Schema RFC 3339 full-time: %s" value))
    (make-decoded-time :second second :minute minute :hour hour
                       :dst -1 :zone zone)))

(defun lg-json-schema--seconds-text (second &optional pad)
  "Return canonical seconds text for numeric SECOND.
Use two whole-second digits when PAD is non-nil."
  (unless (and (numberp second) (<= 0 second) (< second 61))
    (error "Invalid seconds value: %S" second))
  (let* ((whole (truncate second))
         (fraction (- second whole))
         (fraction-text
          (if (zerop fraction)
              ""
            (let ((text (format "%.12f" fraction)))
              (replace-regexp-in-string "0+\\'" "" (substring text 1))))))
    (concat (if pad (format "%02d" whole) (number-to-string whole))
            fraction-text)))

(defun lg-json-schema--time-backward (value)
  "Convert decoded-time VALUE to canonical RFC 3339 full-time text."
  (condition-case nil
      (let ((hour (decoded-time-hour value))
            (minute (decoded-time-minute value))
            (second (decoded-time-second value))
            (zone (decoded-time-zone value)))
        (unless (and (integerp hour) (<= 0 hour 23)
                     (integerp minute) (<= 0 minute 59))
          (error "Invalid decoded full-time"))
        (format "%02d:%02d:%s%s"
                hour minute
                (lg-json-schema--seconds-text second t)
                (lg-json-schema--zone-text zone)))
    (error (error "Expected decoded-time full-time value, got %S" value))))

(defconst lg-json-schema-time-iso
  (lg-iso #'lg-json-schema--time-forward #'lg-json-schema--time-backward)
  "Canonicalizing iso from RFC 3339 full-time to decoded-time lists.")

(defun lg-json-schema--duration-forward (value)
  "Convert RFC 3339 duration VALUE to a decoded-time-style list."
  (unless (and
           (stringp value)
           (or
            (string-match "\\`P\\([0-9]+\\)W\\'" value)
            (string-match
             (concat
              "\\`P\\(?:\\([0-9]+\\)Y\\)?\\(?:\\([0-9]+\\)M\\)?"
              "\\(?:\\([0-9]+\\)D\\)?"
              "\\(?:T\\(?:\\([0-9]+\\)H\\)?\\(?:\\([0-9]+\\)M\\)?"
              "\\(?:\\([0-9]+\\(?:\\.[0-9]+\\)?\\)S\\)?\\)?\\'")
             value)))
    (error "Expected JSON Schema RFC 3339 duration, got %S" value))
  (if (string-match "\\`P\\([0-9]+\\)W\\'" value)
      (make-decoded-time :day (* 7 (string-to-number (match-string 1 value)))
                         :dst -1)
    (string-match
     (concat
      "\\`P\\(?:\\([0-9]+\\)Y\\)?\\(?:\\([0-9]+\\)M\\)?"
      "\\(?:\\([0-9]+\\)D\\)?"
      "\\(?:T\\(?:\\([0-9]+\\)H\\)?\\(?:\\([0-9]+\\)M\\)?"
      "\\(?:\\([0-9]+\\(?:\\.[0-9]+\\)?\\)S\\)?\\)?\\'")
     value)
    (let ((year (and (match-string 1 value)
                     (string-to-number (match-string 1 value))))
          (month (and (match-string 2 value)
                      (string-to-number (match-string 2 value))))
          (day (and (match-string 3 value)
                    (string-to-number (match-string 3 value))))
          (hour (and (match-string 4 value)
                     (string-to-number (match-string 4 value))))
          (minute (and (match-string 5 value)
                       (string-to-number (match-string 5 value))))
          (second (and (match-string 6 value)
                       (string-to-number (match-string 6 value)))))
      (unless (or year month day hour minute second)
        (error "Empty JSON Schema duration: %s" value))
      (make-decoded-time :second second :minute minute :hour hour
                         :day day :month month :year year :dst -1))))

(defun lg-json-schema--duration-part (value suffix)
  "Return duration VALUE followed by SUFFIX, or an empty string."
  (if (or (null value) (and (numberp value) (zerop value)))
      ""
    (unless (and (numberp value) (<= 0 value))
      (error "Invalid duration component: %S" value))
    (format "%s%s" value suffix)))

(defun lg-json-schema--duration-backward (value)
  "Convert decoded-time-style duration VALUE to canonical text."
  (condition-case nil
      (let* ((year (decoded-time-year value))
             (month (decoded-time-month value))
             (day (decoded-time-day value))
             (hour (decoded-time-hour value))
             (minute (decoded-time-minute value))
             (second (decoded-time-second value))
             (date-text
              (concat (lg-json-schema--duration-part year "Y")
                      (lg-json-schema--duration-part month "M")
                      (lg-json-schema--duration-part day "D")))
             (time-text
              (concat (lg-json-schema--duration-part hour "H")
                      (lg-json-schema--duration-part minute "M")
                      (if second
                          (concat (lg-json-schema--seconds-text second) "S")
                        ""))))
        (when (and (string-empty-p date-text) (string-empty-p time-text))
          (setq date-text "0D"))
        (concat "P" date-text
                (if (string-empty-p time-text) "" (concat "T" time-text))))
    (error (error "Expected decoded-time duration value, got %S" value))))

(defconst lg-json-schema-duration-iso
  (lg-iso #'lg-json-schema--duration-forward
          #'lg-json-schema--duration-backward)
  "Canonicalizing iso from RFC 3339 duration to decoded-time lists.")

(defun lg-json-schema--parse-ipv4 (value)
  "Parse dotted IPv4 VALUE into an Emacs network vector."
  (unless (stringp value)
    (error "Expected IPv4 string, got %S" value))
  (let ((parts (split-string value "\\." nil)))
    (unless (and (= (length parts) 4)
                 (cl-every
                  (lambda (part)
                    (and (string-match-p "\\`[0-9]+\\'" part)
                         (<= 0 (string-to-number part) 255)))
                  parts))
      (error "Invalid IPv4 address: %s" value))
    (vconcat (mapcar #'string-to-number parts))))

(defun lg-json-schema--ipv4-backward (value)
  "Convert IPv4 network vector VALUE to canonical dotted text."
  (unless (and (vectorp value) (= (length value) 4)
               (cl-every (lambda (part)
                           (and (integerp part) (<= 0 part 255)))
                         (append value nil)))
    (error "Expected IPv4 network vector, got %S" value))
  (format-network-address value t))

(defconst lg-json-schema-ipv4-iso
  (lg-iso #'lg-json-schema--parse-ipv4 #'lg-json-schema--ipv4-backward)
  "Canonicalizing iso from IPv4 text to Emacs network vectors.")

(defun lg-json-schema--ipv6-piece-values (text)
  "Parse colon-separated IPv6 piece TEXT into numeric words."
  (if (string-empty-p text)
      nil
    (let ((parts (split-string text ":" nil))
          result)
      (dolist (part parts)
        (if (string-match-p "\\." part)
            (let ((ipv4 (lg-json-schema--parse-ipv4 part)))
              (push (+ (* 256 (aref ipv4 0)) (aref ipv4 1)) result)
              (push (+ (* 256 (aref ipv4 2)) (aref ipv4 3)) result))
          (unless (string-match-p "\\`[[:xdigit:]]\\{1,4\\}\\'" part)
            (error "Invalid IPv6 word: %s" part))
          (push (string-to-number part 16) result)))
      (nreverse result))))

(defun lg-json-schema--parse-ipv6 (value)
  "Parse IPv6 VALUE into an Emacs eight-word network vector."
  (unless (stringp value)
    (error "Expected IPv6 string, got %S" value))
  (let* ((compression (string-match "::" value))
         (second-compression
          (and compression (string-match "::" value (+ compression 2)))))
    (when second-compression
      (error "IPv6 address has multiple compression markers: %s" value))
    (let* ((left-text (if compression (substring value 0 compression) value))
           (right-text (if compression (substring value (+ compression 2)) ""))
           (left (lg-json-schema--ipv6-piece-values left-text))
           (right (lg-json-schema--ipv6-piece-values right-text))
           (missing (- 8 (length left) (length right))))
      (unless (if compression (> missing 0) (= missing 0))
        (error "Invalid IPv6 address length: %s" value))
      (vconcat left (make-list missing 0) right))))

(defun lg-json-schema--ipv6-backward (value)
  "Convert IPv6 network vector VALUE to canonical text."
  (unless (and (vectorp value) (= (length value) 8)
               (cl-every (lambda (part)
                           (and (integerp part) (<= 0 part 65535)))
                         (append value nil)))
    (error "Expected IPv6 network vector, got %S" value))
  (format-network-address value t))

(defconst lg-json-schema-ipv6-iso
  (lg-iso #'lg-json-schema--parse-ipv6 #'lg-json-schema--ipv6-backward)
  "Canonicalizing iso from IPv6 text to Emacs network vectors.")

(defun lg-json-schema--url-forward (value require-scheme)
  "Parse URL VALUE, requiring a scheme when REQUIRE-SCHEME is non-nil."
  (unless (stringp value)
    (error "Expected URI string, got %S" value))
  (let ((url (url-generic-parse-url value)))
    (when (and require-scheme (null (url-type url)))
      (error "Expected absolute URI, got %s" value))
    url))

(defun lg-json-schema--url-backward (value)
  "Convert Emacs URL VALUE to canonical text."
  (unless (url-p value)
    (error "Expected Emacs URL value, got %S" value))
  (url-recreate-url value))

(defun lg-json-schema--url-iso (require-scheme)
  "Return URL iso, requiring a scheme when REQUIRE-SCHEME is non-nil."
  (lg-iso (lambda (value)
            (lg-json-schema--url-forward value require-scheme))
          #'lg-json-schema--url-backward))

(defconst lg-json-schema-uri-iso
  (lg-json-schema--url-iso t)
  "Canonicalizing iso from URI text to Emacs URL values.")

(defconst lg-json-schema-uri-reference-iso
  (lg-json-schema--url-iso nil)
  "Canonicalizing iso from URI-reference text to Emacs URL values.")

(defun lg-json-schema--email-forward (value idn)
  "Convert email VALUE to a semantic address, decoding IDN when IDN is non-nil."
  (unless (stringp value)
    (error "Expected email string, got %S" value))
  (let* ((extracted (cadr (mail-extract-address-components value)))
         (at (and extracted (cl-position ?@ extracted :from-end t))))
    (unless (and extracted (string-equal-ignore-case extracted value) at (> at 0)
                 (< at (1- (length extracted))))
      (error "Invalid email address: %s" value))
    (make-lg-json-schema-email
     :local (substring extracted 0 at)
     :domain (downcase
              (if idn
                  (puny-decode-domain (substring extracted (1+ at)))
                (substring extracted (1+ at)))))))

(defun lg-json-schema--email-backward (value idn)
  "Convert semantic email VALUE to text, encoding IDN when IDN is non-nil."
  (unless (lg-json-schema-email-p value)
    (error "Expected semantic email value, got %S" value))
  (let ((local (lg-json-schema-email-local value))
        (domain (lg-json-schema-email-domain value)))
    (unless (and (stringp local) (not (string-empty-p local))
                 (stringp domain) (not (string-empty-p domain)))
      (error "Invalid semantic email value: %S" value))
    (concat local "@" (if idn (puny-encode-domain domain) domain))))

(defun lg-json-schema--email-iso (idn)
  "Return email iso, enabling international domain handling when IDN is non-nil."
  (lg-iso (lambda (value) (lg-json-schema--email-forward value idn))
          (lambda (value) (lg-json-schema--email-backward value idn))))

(defconst lg-json-schema-email-iso
  (lg-json-schema--email-iso nil)
  "Canonicalizing iso from email text to semantic email values.")

(defconst lg-json-schema-idn-email-iso
  (lg-json-schema--email-iso t)
  "Canonicalizing iso from IDN email text to semantic email values.")

(defun lg-json-schema--hostname-forward (value idn)
  "Convert hostname VALUE to a vector of labels, decoding when IDN is non-nil."
  (unless (and (stringp value) (<= (length value) 253))
    (error "Expected hostname string, got %S" value))
  (let* ((decoded (if idn (puny-decode-domain value) value))
         (labels (split-string (downcase decoded) "\\." nil)))
    (unless (and labels
                 (cl-every
                  (lambda (label)
                    (and (<= 1 (length label) 63)
                         (if idn
                             (not (string-match-p "[[:space:]/]" label))
                           (and (string-match-p
                                 "\\`[[:alnum:]]\\(?:[[:alnum:]-]*[[:alnum:]]\\)?\\'"
                                 label)
                                (string-match-p "\\`[[:ascii:]]*\\'" label)))))
                  labels))
      (error "Invalid hostname: %s" value))
    (vconcat labels)))

(defun lg-json-schema--hostname-backward (value idn)
  "Convert hostname label vector VALUE to text, encoding when IDN is non-nil."
  (unless (and (vectorp value)
               (> (length value) 0)
               (cl-every #'stringp (append value nil)))
    (error "Expected hostname label vector, got %S" value))
  (let ((hostname (mapconcat #'identity (append value nil) ".")))
    (if idn (puny-encode-domain hostname) hostname)))

(defun lg-json-schema--hostname-iso (idn)
  "Return hostname iso, enabling international labels when IDN is non-nil."
  (lg-iso (lambda (value) (lg-json-schema--hostname-forward value idn))
          (lambda (value) (lg-json-schema--hostname-backward value idn))))

(defconst lg-json-schema-hostname-iso
  (lg-json-schema--hostname-iso nil)
  "Canonicalizing iso from hostname text to label vectors.")

(defconst lg-json-schema-idn-hostname-iso
  (lg-json-schema--hostname-iso t)
  "Canonicalizing iso from IDN hostname text to Unicode label vectors.")

(defun lg-json-schema--uuid-forward (value)
  "Convert canonical UUID VALUE to a sixteen-byte unibyte string."
  (unless (and
           (stringp value)
           (string-match-p
            (concat "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-"
                    "[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-"
                    "[[:xdigit:]]\\{12\\}\\'")
            value))
    (error "Invalid UUID: %S" value))
  (let ((hex (replace-regexp-in-string "-" "" value))
        (bytes (make-string 16 0))
        (position 0))
    (while (< position 16)
      (aset bytes position
            (string-to-number
             (substring hex (* position 2) (+ (* position 2) 2)) 16))
      (setq position (1+ position)))
    bytes))

(defun lg-json-schema--uuid-backward (value)
  "Convert sixteen-byte unibyte UUID VALUE to canonical text."
  (unless (and (stringp value) (not (multibyte-string-p value))
               (= (length value) 16))
    (error "Expected sixteen-byte UUID string, got %S" value))
  (let ((hex (mapconcat (lambda (byte) (format "%02x" byte))
                        (append value nil) "")))
    (format "%s-%s-%s-%s-%s"
            (substring hex 0 8) (substring hex 8 12)
            (substring hex 12 16) (substring hex 16 20)
            (substring hex 20 32))))

(defconst lg-json-schema-uuid-iso
  (lg-iso #'lg-json-schema--uuid-forward #'lg-json-schema--uuid-backward)
  "Canonicalizing iso from UUID text to sixteen-byte strings.")

(defun lg-json-schema--regex-forward (value)
  "Translate practical ECMA-262 regex VALUE into an Emacs regexp wrapper."
  (unless (stringp value)
    (error "Expected JSON Schema regex string, got %S" value))
  (let ((index 0)
        (length (length value))
        (class-p nil)
        parts)
    (while (< index length)
      (let ((character (aref value index)))
        (cond
         ((= character ?\[)
          (setq class-p t)
          (push "[" parts))
         ((and class-p (= character ?\]))
          (setq class-p nil)
          (push "]" parts))
         ((= character ?\\)
          (setq index (1+ index))
          (when (= index length)
            (error "Trailing escape in JSON Schema regex: %s" value))
          (let ((escaped (aref value index)))
            (push
             (pcase escaped
               (?d (if class-p "0-9" "[0-9]"))
               (?D (if class-p (error "Unsupported \\D inside character class")
                     "[^0-9]"))
               (?w (if class-p "A-Za-z0-9_" "[A-Za-z0-9_]"))
               (?W (if class-p (error "Unsupported \\W inside character class")
                     "[^A-Za-z0-9_]"))
               (?s (if class-p "[:space:]" "[[:space:]]"))
               (?S (if class-p (error "Unsupported \\S inside character class")
                     "[^[:space:]]"))
               ((or ?p ?P)
                (error "Unicode property escapes are not supported"))
               ((or ?b ?B ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
                (concat "\\" (char-to-string escaped)))
               (_ (pcase escaped
                    ((or ?+ ??) (concat "\\" (char-to-string escaped)))
                    ((or ?| ?\( ?\) ?{ ?}) (char-to-string escaped))
                    (_ (concat "\\" (char-to-string escaped))))))
             parts)))
         (class-p (push (char-to-string character) parts))
         ((and (= character ?\() (< (+ index 2) length)
               (= (aref value (1+ index)) ??)
               (memq (aref value (+ index 2)) '(?: ?= ?!)))
          (push (concat "\\(?" (char-to-string (aref value (+ index 2)))) parts)
          (setq index (+ index 2)))
         ((and (= character ?\() (< index (1- length))
               (= (aref value (1+ index)) ??))
          (error "Unsupported ECMA-262 group in JSON Schema regex: %s" value))
         ((memq character '(?\( ?\)))
          (push (concat "\\" (char-to-string character)) parts))
         ((memq character '(?| ?{ ?}))
          (push (concat "\\" (char-to-string character)) parts))
         (t (push (char-to-string character) parts))))
      (setq index (1+ index)))
    (when class-p
      (error "Unclosed character class in JSON Schema regex: %s" value))
    (make-lg-json-schema-regex
     :source value
     :emacs-regexp (apply #'concat (nreverse parts)))))

(defun lg-json-schema--regex-backward (value)
  "Return ECMA-262 source from semantic regex VALUE."
  (unless (lg-json-schema-regex-p value)
    (error "Expected semantic JSON Schema regex, got %S" value))
  (lg-json-schema-regex-source value))

(defun lg-json-schema-regex-match-p (regex string &optional start)
  "Return non-nil when semantic REGEX matches STRING at or after START."
  (unless (lg-json-schema-regex-p regex)
    (error "Expected semantic JSON Schema regex, got %S" regex))
  (string-match-p (lg-json-schema-regex-emacs-regexp regex) string start))

(defconst lg-json-schema-regex-iso
  (lg-iso #'lg-json-schema--regex-forward #'lg-json-schema--regex-backward)
  "Iso from supported ECMA-262 regex text to matchable Emacs regex wrappers.")

(defun lg-json-schema--pointer-token-forward (token)
  "Decode JSON Pointer TOKEN or signal on an invalid escape."
  (when (string-match-p "~\\(?:[^01]\\|\\'\\)" token)
    (error "Invalid JSON Pointer escape in token: %s" token))
  (replace-regexp-in-string
   "~1" "/" (replace-regexp-in-string "~0" "~" token t t) t t))

(defun lg-json-schema--pointer-token-backward (token)
  "Encode JSON Pointer TOKEN."
  (unless (stringp token)
    (error "Expected JSON Pointer string token, got %S" token))
  (replace-regexp-in-string
   "/" "~1" (replace-regexp-in-string "~" "~0" token t t) t t))

(defun lg-json-schema--pointer-forward (value)
  "Convert JSON Pointer VALUE to decoded tokens."
  (unless (stringp value)
    (error "Expected JSON Pointer string, got %S" value))
  (make-lg-json-schema-pointer
   :tokens
   (if (string-empty-p value)
       nil
     (unless (string-prefix-p "/" value)
       (error "Invalid JSON Pointer: %s" value))
     (mapcar #'lg-json-schema--pointer-token-forward
             (split-string (substring value 1) "/" nil)))))

(defun lg-json-schema--pointer-backward (value)
  "Convert semantic JSON Pointer VALUE to text."
  (unless (lg-json-schema-pointer-p value)
    (error "Expected semantic JSON Pointer, got %S" value))
  (mapconcat (lambda (token)
               (concat "/" (lg-json-schema--pointer-token-backward token)))
             (lg-json-schema-pointer-tokens value) ""))

(defconst lg-json-schema-json-pointer-iso
  (lg-iso #'lg-json-schema--pointer-forward #'lg-json-schema--pointer-backward)
  "Iso from JSON Pointer text to decoded token lists.")

(defun lg-json-schema--relative-pointer-forward (value)
  "Convert relative JSON Pointer VALUE to a semantic pointer."
  (unless (and (stringp value)
               (string-match "\\`\\(0\\|[1-9][0-9]*\\)\\(.*\\)\\'" value))
    (error "Invalid relative JSON Pointer: %S" value))
  (let ((up (string-to-number (match-string 1 value)))
        (remainder (match-string 2 value)))
    (cond
     ((string= remainder "#")
      (make-lg-json-schema-relative-pointer
       :up up :query-index-p t :tokens nil))
     ((or (string-empty-p remainder) (string-prefix-p "/" remainder))
      (make-lg-json-schema-relative-pointer
       :up up :query-index-p nil
       :tokens (lg-json-schema-pointer-tokens
                (lg-json-schema--pointer-forward remainder))))
     (t (error "Invalid relative JSON Pointer: %s" value)))))

(defun lg-json-schema--relative-pointer-backward (value)
  "Convert semantic relative JSON Pointer VALUE to text."
  (unless (and (lg-json-schema-relative-pointer-p value)
               (integerp (lg-json-schema-relative-pointer-up value))
               (>= (lg-json-schema-relative-pointer-up value) 0))
    (error "Expected semantic relative JSON Pointer, got %S" value))
  (concat
   (number-to-string (lg-json-schema-relative-pointer-up value))
   (if (lg-json-schema-relative-pointer-query-index-p value)
       "#"
     (lg-json-schema--pointer-backward
      (make-lg-json-schema-pointer
       :tokens (lg-json-schema-relative-pointer-tokens value))))))

(defconst lg-json-schema-relative-json-pointer-iso
  (lg-iso #'lg-json-schema--relative-pointer-forward
          #'lg-json-schema--relative-pointer-backward)
  "Iso from relative JSON Pointer text to semantic relative pointers.")

(defun lg-json-schema--uri-template-forward (value)
  "Convert URI template VALUE to source plus referenced variable names."
  (unless (stringp value)
    (error "Expected URI template string, got %S" value))
  (let ((index 0)
        variables)
    (while (string-match "{\\([^{}]+\\)}" value index)
      (let* ((expression (match-string 1 value))
             (body (if (string-match-p "\\`[+#./;?&]" expression)
                       (substring expression 1)
                     expression)))
        (dolist (variable (split-string body "," t))
          (let ((name (car (split-string variable "[:*]"))))
            (unless (string-match-p "\\`[A-Za-z0-9_.%]+\\'" name)
              (error "Invalid URI template variable: %s" name))
            (push name variables))))
      (setq index (match-end 0)))
    (when (or (string-match-p "[{}]" (replace-regexp-in-string "{[^{}]+}" "" value))
              (and (string-match-p "{" value) (null variables)))
      (error "Invalid URI template: %s" value))
    (make-lg-json-schema-uri-template
     :source value :variables (nreverse variables))))

(defun lg-json-schema--uri-template-backward (value)
  "Return source text from semantic URI template VALUE."
  (unless (lg-json-schema-uri-template-p value)
    (error "Expected semantic URI template, got %S" value))
  (lg-json-schema-uri-template-source value))

(defconst lg-json-schema-uri-template-iso
  (lg-iso #'lg-json-schema--uri-template-forward
          #'lg-json-schema--uri-template-backward)
  "Iso from URI template text to source-and-variable records.")

(defun lg-json-schema--base64-forward (value)
  "Decode Base64 string VALUE to unibyte data."
  (unless (stringp value)
    (error "Expected Base64 string, got %S" value))
  (condition-case nil
      (base64-decode-string value)
    (error (error "Invalid Base64 data"))))

(defun lg-json-schema--base64-backward (value)
  "Encode byte string VALUE as canonical Base64 text."
  (unless (and (stringp value) (not (multibyte-string-p value)))
    (error "Expected unibyte data for Base64 encoding, got %S" value))
  (base64-encode-string value t))

(defconst lg-json-schema-base64-iso
  (lg-iso #'lg-json-schema--base64-forward
          #'lg-json-schema--base64-backward)
  "Canonicalizing iso from Base64 text to unibyte data.")

(defconst lg-json-schema-default-formats
  `(("date" . ,lg-json-schema-date-iso)
    ("date-time" . ,lg-json-schema-date-time-iso)
    ("time" . ,lg-json-schema-time-iso)
    ("duration" . ,lg-json-schema-duration-iso)
    ("email" . ,lg-json-schema-email-iso)
    ("idn-email" . ,lg-json-schema-idn-email-iso)
    ("hostname" . ,lg-json-schema-hostname-iso)
    ("idn-hostname" . ,lg-json-schema-idn-hostname-iso)
    ("ipv4" . ,lg-json-schema-ipv4-iso)
    ("ipv6" . ,lg-json-schema-ipv6-iso)
    ("uri" . ,lg-json-schema-uri-iso)
    ("uri-reference" . ,lg-json-schema-uri-reference-iso)
    ("iri" . ,lg-json-schema-uri-iso)
    ("iri-reference" . ,lg-json-schema-uri-reference-iso)
    ("uuid" . ,lg-json-schema-uuid-iso)
    ("regex" . ,lg-json-schema-regex-iso)
    ("json-pointer" . ,lg-json-schema-json-pointer-iso)
    ("relative-json-pointer" . ,lg-json-schema-relative-json-pointer-iso)
    ("uri-template" . ,lg-json-schema-uri-template-iso))
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

(defun lg-json-schema--content-converter (schema)
  "Return the annotated content-encoding converter for SCHEMA."
  (let ((encoding (lg-json-schema--member schema "contentEncoding")))
    (cond
     ((eq encoding (lg-json-schema--missing)) lg-id)
     ((and (stringp encoding) (string-equal-ignore-case encoding "base64"))
      lg-json-schema-base64-iso)
     (t lg-id))))

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
             (lg-json-schema--content-converter schema)
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
