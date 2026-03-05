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

(cl-defstruct lg-buffer-op-point-get)
(cl-defstruct lg-buffer-op-point-set point)
(cl-defstruct lg-buffer-op-mark-get)
(cl-defstruct lg-buffer-op-mark-set mark)
(cl-defstruct lg-buffer-op-string-get)
(cl-defstruct lg-buffer-op-string-set text)
(cl-defstruct lg-buffer-op-region-get)
(cl-defstruct lg-buffer-op-region-set text)
(cl-defstruct lg-buffer-op-substring-get start end)
(cl-defstruct lg-buffer-op-substring-set start end text)

(cl-defmethod lg-effect-handle ((buffer buffer) (_op lg-buffer-op-point-get))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (point)))

(cl-defmethod lg-effect-handle ((buffer buffer) (op lg-buffer-op-point-set))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (goto-char (max (point-min)
                    (min (point-max) (lg-buffer-op-point-set-point op)))))
  buffer)

(cl-defmethod lg-effect-handle ((buffer buffer) (_op lg-buffer-op-mark-get))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (mark t)))

(cl-defmethod lg-effect-handle ((buffer buffer) (op lg-buffer-op-mark-set))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (if (lg-buffer-op-mark-set-mark op)
        (set-mark (lg-buffer-op-mark-set-mark op))
      (set-mark nil)))
  buffer)

(cl-defmethod lg-effect-handle ((buffer buffer) (_op lg-buffer-op-string-get))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defmethod lg-effect-handle ((buffer buffer) (op lg-buffer-op-string-set))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (erase-buffer)
    (insert (lg-buffer-op-string-set-text op)))
  buffer)

(cl-defmethod lg-effect-handle ((buffer buffer) (_op lg-buffer-op-region-get))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (if (mark t)
        (let ((start (min (point) (mark t)))
              (end (max (point) (mark t))))
          (lg-just (buffer-substring-no-properties start end)))
      lg-nothing)))

(cl-defmethod lg-effect-handle ((buffer buffer) (op lg-buffer-op-region-set))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (if (mark t)
        (let ((start (min (point) (mark t)))
              (end (max (point) (mark t)))
              (text (lg-buffer-op-region-set-text op)))
          (delete-region start end)
          (goto-char start)
          (insert text)
          (set-mark (+ start (length text))))
      buffer))
  buffer)

(cl-defmethod lg-effect-handle ((buffer buffer) (op lg-buffer-op-substring-get))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (buffer-substring-no-properties
     (lg-buffer-op-substring-get-start op)
     (lg-buffer-op-substring-get-end op))))

(cl-defmethod lg-effect-handle ((buffer buffer) (op lg-buffer-op-substring-set))
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (goto-char (lg-buffer-op-substring-set-start op))
    (delete-region (lg-buffer-op-substring-set-start op)
                   (lg-buffer-op-substring-set-end op))
    (insert (lg-buffer-op-substring-set-text op)))
  buffer)

(defconst lg-buffer-point
  (lg-mark-impure
   (lg-lens
    (lambda (buffer)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-point-get))
       buffer))
    (lambda (buffer new-point)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-point-set :point new-point))
       buffer))))
  "Lens focusing point position in a buffer.")

(defconst lg-buffer-mark
  (lg-mark-impure
   (lg-lens
    (lambda (buffer)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-mark-get))
       buffer))
    (lambda (buffer new-mark)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-mark-set :mark new-mark))
       buffer))))
  "Lens focusing mark position in a buffer.")

(defconst lg-buffer-string
  (lg-mark-impure
   (lg-lens
    (lambda (buffer)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-string-get))
       buffer))
    (lambda (buffer new-contents)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-string-set :text new-contents))
       buffer))))
  "Lens focusing full text contents of a buffer.")

(defconst lg-buffer-region-string
  (lg-mark-impure
   (lg-affine
    (lambda (buffer)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-region-get))
       buffer))
    (lambda (buffer new-text)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-region-set :text new-text))
       buffer))))
  "Affine traversal focusing active region text in a buffer.")

(defun lg-buffer-substring (start end)
  "Lens focusing text in fixed START/END buffer positions."
  (lg-mark-impure
   (lg-lens
    (lambda (buffer)
      (lg-run-effect
       (lg-effect-perform (make-lg-buffer-op-substring-get :start start :end end))
       buffer))
    (lambda (buffer new-text)
      (lg-run-effect
       (lg-effect-perform
        (make-lg-buffer-op-substring-set :start start :end end :text new-text))
       buffer)))))

(provide 'looking-glass-buffer)

;;; looking-glass-buffer.el ends here
