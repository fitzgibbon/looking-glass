;;; looking-glass-buffer.el --- Buffer optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6") (looking-glass "0.1.0"))
;; Keywords: lisp, convenience, extensions

;;; Commentary:

;; Buffer/window/point/region-oriented optics.

;;; Code:

(require 'looking-glass)

(defun lg-buffer--live-buffer (value)
  "Return VALUE when it is a live buffer, or signal otherwise."
  (if (buffer-live-p value)
      value
    (error "Expected live buffer")))

(defconst lg-buffer-point
  (lg-lens
   (lambda (buffer)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (point)))
   (lambda (buffer new-point)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (goto-char (max (point-min)
                       (min (point-max) new-point))))
     buffer))
  "Lens focusing point position in a buffer.")

(defconst lg-buffer-mark
  (lg-lens
   (lambda (buffer)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (mark t)))
   (lambda (buffer new-mark)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (if new-mark
           (set-mark new-mark)
         (set-mark nil)))
     buffer))
  "Lens focusing mark position in a buffer.")

(defconst lg-buffer-string
  (lg-lens
   (lambda (buffer)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (buffer-substring-no-properties (point-min) (point-max))))
   (lambda (buffer new-contents)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (erase-buffer)
       (insert new-contents))
     buffer))
  "Lens focusing full text contents of a buffer.")

(defconst lg-buffer-region-string
  (lg-affine
   (lambda (buffer)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (if (mark t)
           (let ((start (min (point) (mark t)))
                 (end (max (point) (mark t))))
             (lg-just (buffer-substring-no-properties start end)))
         lg-nothing)))
   (lambda (buffer new-text)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (if (mark t)
           (let ((start (min (point) (mark t)))
                 (end (max (point) (mark t))))
             (delete-region start end)
             (goto-char start)
             (insert new-text)
             (set-mark (+ start (length new-text))))
         buffer))
     buffer))
  "Affine traversal focusing active region text in a buffer.")

(defun lg-buffer-substring (start end)
  "Lens focusing text in fixed START/END buffer positions."
  (lg-lens
   (lambda (buffer)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (buffer-substring-no-properties start end)))
   (lambda (buffer new-text)
     (with-current-buffer (lg-buffer--live-buffer buffer)
       (goto-char start)
       (delete-region start end)
       (insert new-text))
     buffer)))

(provide 'looking-glass-buffer)

;;; looking-glass-buffer.el ends here
