;;; looking-glass-regex.el --- Regex optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6") (looking-glass "0.1.0"))
;; Keywords: lisp, convenience, regex

;;; Commentary:

;; Regex-oriented optics built on top of looking-glass.

;;; Code:

(require 'cl-lib)
(require 'looking-glass)

(defun lg-regex--ensure-string (value)
  "Return VALUE when it is a string, or signal otherwise."
  (if (stringp value)
      value
    (error "Expected string source")))

(defun lg-regex--utf8-roundtrip-ok-p (bytes text)
  "Return non-nil when TEXT encodes back to BYTES as UTF-8."
  (and (multibyte-string-p text)
       (equal bytes (encode-coding-string text 'utf-8 t))))

(defun lg-regex--utf8-bytes-p (bytes)
  "Return non-nil when BYTES is detected as UTF-8 data."
  (let ((coding (detect-coding-string bytes t)))
    (and coding
         (eq (coding-system-base coding) 'utf-8))))

(defun lg-regex--collect-spans (regexp source group)
  "Collect non-overlapping REGEXP match spans in SOURCE for GROUP.
Each result is a list (BEG END TEXT)."
  (let ((start 0)
        (length (length source))
        spans)
    (while (and (<= start length)
                (string-match regexp source start))
      (let ((match-beg (match-beginning 0))
            (match-end (match-end 0))
            (beg (match-beginning group))
            (end (match-end group)))
        (when (and beg end)
          (push (list beg end (substring source beg end)) spans))
        (setq start
              (if (= match-beg match-end)
                  (1+ match-end)
                match-end))))
    (nreverse spans)))

(defun lg-regex--replace-spans (source spans replacements)
  "Replace SPANS in SOURCE with REPLACEMENTS.
SPANS is a list of (BEG END TEXT). REPLACEMENTS aligns 1:1 with SPANS."
  (let ((cursor 0)
        (parts nil))
    (cl-mapc (lambda (span replacement)
               (let ((beg (nth 0 span))
                     (end (nth 1 span)))
                 (push (substring source cursor beg) parts)
                 (push replacement parts)
                 (setq cursor end)))
             spans
             replacements)
    (push (substring source cursor) parts)
    (apply #'concat (nreverse parts))))

(defconst lg-regex--line-separator-regexp
  (regexp-opt (list "\r\n"
                    "\n"
                    "\r"
                    "\v"
                    "\f"
                    (string #x85)
                    (string #x2028)
                    (string #x2029)))
  "Regexp matching common Unicode line separators.")

(defun lg-regex--collect-line-spans (source)
  "Collect line-content spans in SOURCE as (BEG END TEXT).
Separators are not included in spans and are preserved during replacement."
  (let ((start 0)
        (length (length source))
        spans)
    (while (and (<= start length)
                (string-match lg-regex--line-separator-regexp source start))
      (let ((sep-beg (match-beginning 0))
            (sep-end (match-end 0)))
        (push (list start sep-beg (substring source start sep-beg)) spans)
        (setq start sep-end)))
    (push (list start length (substring source start length)) spans)
    (nreverse spans)))

(defun lg-regex--traverse-list (applicative afb source)
  "Traverse SOURCE list with AFB using APPLICATIVE."
  (let ((pure (lg-applicative-pure applicative))
        (ap (lg-applicative-ap applicative))
        (fmap (lg-applicative-fmap applicative)))
    (funcall fmap
             #'nreverse
             (cl-reduce
              (lambda (acc focus)
                (funcall ap
                         (funcall fmap
                                  (lambda (partial)
                                    (lambda (item) (cons item partial)))
                                  acc)
                         (funcall afb focus)))
              source
              :initial-value (funcall pure nil)))))

(defun lg-regex-match (regexp)
  "Affine traversal focusing the first full REGEXP match in a string."
  (lg-affine
   (lambda (source)
     (setq source (lg-regex--ensure-string source))
     (let ((spans (lg-regex--collect-spans regexp source 0)))
       (if spans
           (lg-just (nth 2 (car spans)))
         lg-nothing)))
   (lambda (source new-focus)
     (setq source (lg-regex--ensure-string source))
     (let ((spans (lg-regex--collect-spans regexp source 0)))
       (if spans
           (lg-regex--replace-spans source (list (car spans)) (list new-focus))
         source)))))

(defun lg-regex-group (regexp group)
  "Affine traversal focusing first REGEXP capture GROUP in a string."
  (lg-affine
   (lambda (source)
     (setq source (lg-regex--ensure-string source))
     (let ((spans (lg-regex--collect-spans regexp source group)))
       (if spans
           (lg-just (nth 2 (car spans)))
         lg-nothing)))
   (lambda (source new-focus)
     (setq source (lg-regex--ensure-string source))
     (let ((spans (lg-regex--collect-spans regexp source group)))
       (if spans
           (lg-regex--replace-spans source (list (car spans)) (list new-focus))
         source)))))

(defun lg-regex-all (regexp)
  "Traversal focusing all non-overlapping full REGEXP matches in a string."
  (lg-traversal
   (lambda (afb source applicative)
     (setq source (lg-regex--ensure-string source))
     (let* ((fmap (lg-applicative-fmap applicative))
            (spans (lg-regex--collect-spans regexp source 0))
            (focuses (mapcar (lambda (span) (nth 2 span)) spans)))
       (funcall fmap
                (lambda (replacements)
                  (lg-regex--replace-spans source spans replacements))
                (lg-regex--traverse-list applicative afb focuses))))))

(defun lg-regex-all-groups (regexp group)
  "Traversal focusing capture GROUP for each REGEXP match in a string."
  (lg-traversal
   (lambda (afb source applicative)
     (setq source (lg-regex--ensure-string source))
     (let* ((fmap (lg-applicative-fmap applicative))
            (spans (lg-regex--collect-spans regexp source group))
            (focuses (mapcar (lambda (span) (nth 2 span)) spans)))
       (funcall fmap
                (lambda (replacements)
                  (lg-regex--replace-spans source spans replacements))
                (lg-regex--traverse-list applicative afb focuses))))))

(defun lg-regex-replace (regexp replacement source)
  "Replace all REGEXP matches in SOURCE with REPLACEMENT.
REPLACEMENT is a function called with each match string."
  (lg-over (lg-regex-all regexp) replacement source))

(defun lg-regex-replace-group (regexp group replacement source)
  "Replace REGEXP capture GROUP matches in SOURCE using REPLACEMENT."
  (lg-over (lg-regex-all-groups regexp group) replacement source))

(defconst lg-worded
  (lg-regex-all "\\w+")
  "Traversal focusing word-like segments using Emacs regex `\\w+'.")

(defun lg-worded-regexp (regexp)
  "Traversal focusing word-like segments matching REGEXP."
  (lg-regex-all regexp))

(defconst lg-lined
  (lg-traversal
   (lambda (afb source applicative)
     (setq source (lg-regex--ensure-string source))
     (let* ((fmap (lg-applicative-fmap applicative))
            (spans (lg-regex--collect-line-spans source))
            (focuses (mapcar (lambda (span) (nth 2 span)) spans)))
       (funcall fmap
                (lambda (replacements)
                  (lg-regex--replace-spans source spans replacements))
                (lg-regex--traverse-list applicative afb focuses)))))
  "Traversal focusing line content while preserving original separators.")

(defconst lg-utf8
  (lg-prism
   (lambda (source)
     (if (and (stringp source)
              (not (multibyte-string-p source))
              (lg-regex--utf8-bytes-p source))
         (condition-case nil
             (let ((text (decode-coding-string source 'utf-8)))
               (if (lg-regex--utf8-roundtrip-ok-p source text)
                   (lg-right text)
                 (lg-left source)))
           (error
            (lg-left source)))
       (lg-left source)))
   (lambda (text)
     (encode-coding-string (lg-regex--ensure-string text) 'utf-8 t)))
  "Prism between unibyte UTF-8 bytestrings and decoded text strings.")

(provide 'looking-glass-regex)

;;; looking-glass-regex.el ends here
