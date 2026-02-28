;;; looking-glass-buffer.el --- Buffer optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (looking-glass "0.4.0"))
;; Keywords: lisp, data, buffer, tools
;; URL: https://github.com/fitzgibbon/looking-glass

;;; Commentary:

;; Buffer-focused optics built on top of looking-glass.

;;; Code:

(require 'looking-glass)

(defun lg-buffer--ensure-live (buffer)
  "Return BUFFER when it is live, else signal an error."
  (unless (buffer-live-p buffer)
    (error "Expected live buffer, got: %S" buffer))
  buffer)

(defun lg-buffer--ensure-string (value context)
  "Signal when VALUE is not a string for CONTEXT."
  (unless (stringp value)
    (error "%s expects string replacement, got: %S" context value))
  value)

(defun lg-buffer--replace-region (buffer beg end text)
  "Replace in BUFFER region BEG..END with TEXT and return BUFFER."
  (lg-buffer--ensure-live buffer)
  (lg-buffer--ensure-string text "Buffer optic")
  (with-current-buffer buffer
    (let ((saved-point (point))
          (saved-mark (mark t)))
      (save-restriction
        (save-excursion
          (delete-region beg end)
          (goto-char beg)
          (insert text)))
      (goto-char (min (max (point-min) saved-point) (point-max)))
      (set-marker (mark-marker)
                  (and saved-mark
                       (min (max (point-min) saved-mark) (point-max)))
                  buffer)))
  buffer)

(defun lg-buffer--accessible-lines (buffer)
  "Return list of accessible line strings from BUFFER.

Each element contains its original trailing newline when present."
  (lg-buffer--ensure-live buffer)
  (with-current-buffer buffer
    (let ((out nil)
          (min (point-min))
          (max (point-max)))
      (save-restriction
        (save-excursion
          (goto-char min)
          (while (< (point) max)
            (let* ((start (point))
                   (line-end (line-end-position))
                   (next (if (< line-end max)
                             (1+ line-end)
                           max)))
              (push (buffer-substring-no-properties start next) out)
              (goto-char next)))))
      (nreverse out))))

(defun lg-buffer--replace-accessible (buffer text)
  "Replace the accessible portion of BUFFER with TEXT and return BUFFER."
  (lg-buffer--ensure-live buffer)
  (lg-buffer--ensure-string text "`lg-buffer-string'")
  (with-current-buffer buffer
    (let ((min (point-min))
          (max (point-max)))
      (lg-buffer--replace-region buffer min max text))))

(defun lg-buffer-string ()
  "Lens focusing the accessible text of a live buffer.

The focus is the text between `point-min' and `point-max'."
  (lg-lens
   (lambda (source)
     (lg-buffer--ensure-live source)
     (with-current-buffer source
       (buffer-substring-no-properties (point-min) (point-max))))
   (lambda (new source)
     (lg-buffer--replace-accessible source new))))

(defun lg-buffer-region (beg end)
  "Lens focusing fixed region BEG..END in a live buffer."
  (unless (and (integerp beg) (integerp end) (<= beg end))
    (error "BEG and END must be integers where BEG <= END"))
  (lg-lens
   (lambda (source)
     (lg-buffer--ensure-live source)
     (with-current-buffer source
       (buffer-substring-no-properties beg end)))
   (lambda (new source)
     (lg-buffer--replace-region source beg end new))))

(defun lg-buffer-lines ()
  "Traversal over accessible buffer lines as strings.

Each focused line includes its trailing newline when present."
  (lg-wander
   (lambda (step source app)
     (lg-buffer--ensure-live source)
     (let ((pure (lg-applicative-pure app))
           (map2 (lg-applicative-map2 app))
           (acc nil))
       (setq acc (funcall pure nil))
       (dolist (line (lg-buffer--accessible-lines source))
         (setq acc
               (funcall map2
                        (lambda (xs y)
                          (append xs (list y)))
                        acc
                        (funcall step line))))
       (lg--app-map app
                    (lambda (updated-lines)
                      (lg-buffer--replace-accessible source
                                                    (mapconcat #'identity
                                                               updated-lines
                                                               "")))
                    acc)))))

(defun lg-ibuffer-lines ()
  "Indexed traversal over accessible buffer lines.

Indices are zero-based line ordinals within the accessible region."
  (lg-iwander
   (lambda (step source app)
     (lg-buffer--ensure-live source)
     (let ((pure (lg-applicative-pure app))
           (map2 (lg-applicative-map2 app))
           (acc nil)
           (idx 0))
       (setq acc (funcall pure nil))
       (dolist (line (lg-buffer--accessible-lines source))
         (let ((line-idx idx))
           (setq acc
                 (funcall map2
                          (lambda (xs idx+line)
                            (append xs (list idx+line)))
                          acc
                          (funcall step (cons line-idx line)))))
         (setq idx (1+ idx)))
       (lg--app-map app
                    (lambda (updated)
                      (lg-buffer--replace-accessible
                       source
                       (mapconcat #'identity (mapcar #'cdr updated) "")))
                    acc)))))

(defun lg-buffer-matches (regexp &optional subgroup)
  "Traversal over REGEXP matches in accessible buffer text.

SUBGROUP selects capture group focus and defaults to 0."
  (lg-compose (lg-buffer-string) (lg-regex regexp subgroup)))

(defun lg-buffer--istring ()
  "Indexed lens variant of `lg-buffer-string' for internal composition."
  (lg-ilens
   (lambda (source)
     (lg-buffer--ensure-live source)
     (with-current-buffer source
       (buffer-substring-no-properties (point-min) (point-max))))
   (lambda (new source)
     (lg-buffer--replace-accessible source new))
   (lambda (_source) nil)))

(defun lg-ibuffer-matches (regexp &optional subgroup)
  "Indexed traversal over REGEXP matches in accessible buffer text.

Indices are zero-based match ordinals where SUBGROUP exists."
  (lg-compose (lg-buffer--istring) (lg-iregex regexp subgroup)))

(defun lg-buffer-name ()
  "Lens focusing a live buffer name."
  (lg-lens
   (lambda (source)
     (lg-buffer--ensure-live source)
     (buffer-name source))
   (lambda (new source)
     (lg-buffer--ensure-live source)
     (lg-buffer--ensure-string new "`lg-buffer-name'")
     (with-current-buffer source
       (rename-buffer new t)
       source))))

(defun lg-buffer-modified-p ()
  "Lens focusing modified state of a live buffer."
  (lg-lens
   (lambda (source)
     (lg-buffer--ensure-live source)
     (buffer-modified-p source))
   (lambda (new source)
     (lg-buffer--ensure-live source)
     (with-current-buffer source
       (set-buffer-modified-p new)
       source))))

(defun lg-buffer-point ()
  "Lens focusing point in a live buffer.

Setting moves point in SOURCE to the new absolute position."
  (lg-lens
   (lambda (source)
     (lg-buffer--ensure-live source)
     (with-current-buffer source
       (point)))
   (lambda (new source)
     (lg-buffer--ensure-live source)
     (unless (integerp new)
       (error "`lg-buffer-point' expects integer position, got: %S" new))
     (with-current-buffer source
       (goto-char new)
       source))))

(defun lg-buffer-mark ()
  "Lens focusing mark in a live buffer.

The focus is the mark position as an integer or nil when unset."
  (lg-lens
   (lambda (source)
     (lg-buffer--ensure-live source)
     (with-current-buffer source
       (mark t)))
   (lambda (new source)
     (lg-buffer--ensure-live source)
     (unless (or (null new) (integerp new) (markerp new))
       (error "`lg-buffer-mark' expects nil or buffer position, got: %S" new))
     (with-current-buffer source
       (set-marker (mark-marker) new source)
       source))))

(provide 'looking-glass-buffer)

;;; looking-glass-buffer.el ends here
