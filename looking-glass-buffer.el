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

(cl-defstruct lg-buffer--state
  text
  point
  mark)

(defun lg-buffer--bounds-for-text (text)
  "Return (MIN . MAX) bounds for TEXT as buffer-like positions."
  (cons 1 (1+ (length text))))

(defun lg-buffer--clamp-position (position text)
  "Clamp POSITION to valid bounds for TEXT."
  (let* ((bounds (lg-buffer--bounds-for-text text))
         (min-pos (car bounds))
         (max-pos (cdr bounds)))
    (max min-pos (min max-pos position))))

(defun lg-buffer--capture-state (buffer)
  "Capture immutable snapshot state from live BUFFER."
  (with-current-buffer (lg-buffer--live-buffer buffer)
    (make-lg-buffer--state
     :text (buffer-substring-no-properties (point-min) (point-max))
     :point (point)
     :mark (mark t))))

(defun lg-buffer--apply-state (buffer state)
  "Apply immutable snapshot STATE into live BUFFER."
  (let* ((text (lg-buffer--state-text state))
         (clamped-point (lg-buffer--clamp-position (lg-buffer--state-point state) text))
         (mark-value (lg-buffer--state-mark state))
         (clamped-mark (when mark-value
                         (lg-buffer--clamp-position mark-value text))))
    (with-current-buffer (lg-buffer--live-buffer buffer)
      (erase-buffer)
      (insert text)
      (goto-char clamped-point)
      (set-mark clamped-mark))
    buffer))

(cl-defmethod lg-ref-read ((buffer buffer))
  (lg-buffer--capture-state buffer))

(cl-defmethod lg-ref-write ((buffer buffer) (state lg-buffer--state))
  (lg-buffer--apply-state buffer state))

(defconst lg-buffer--point-o
  (lg-lens
   #'lg-buffer--state-point
   (lambda (state new-point)
     (make-lg-buffer--state
      :text (lg-buffer--state-text state)
      :point (lg-buffer--clamp-position new-point (lg-buffer--state-text state))
      :mark (lg-buffer--state-mark state)))))

(defconst lg-buffer--mark-o
  (lg-lens
   #'lg-buffer--state-mark
   (lambda (state new-mark)
     (make-lg-buffer--state
      :text (lg-buffer--state-text state)
      :point (lg-buffer--state-point state)
      :mark (when new-mark
              (lg-buffer--clamp-position new-mark (lg-buffer--state-text state)))))))

(defconst lg-buffer--string-o
  (lg-lens
   #'lg-buffer--state-text
   (lambda (state new-text)
     (make-lg-buffer--state
      :text new-text
      :point (lg-buffer--clamp-position (lg-buffer--state-point state) new-text)
      :mark (let ((mark-value (lg-buffer--state-mark state)))
              (when mark-value
                (lg-buffer--clamp-position mark-value new-text)))))))

(defconst lg-buffer--region-string-o
  (lg-affine
   (lambda (state)
     (let ((mark-value (lg-buffer--state-mark state)))
       (if (null mark-value)
           lg-nothing
         (let* ((text (lg-buffer--state-text state))
                (point-value (lg-buffer--state-point state))
                (start (min point-value mark-value))
                (end (max point-value mark-value)))
           (lg-just (substring text (1- start) (1- end)))))))
   (lambda (state new-text)
     (let ((mark-value (lg-buffer--state-mark state)))
       (if (null mark-value)
           state
         (let* ((text (lg-buffer--state-text state))
                (point-value (lg-buffer--state-point state))
                (start (min point-value mark-value))
                (end (max point-value mark-value))
                (replaced (concat (substring text 0 (1- start))
                                  new-text
                                  (substring text (1- end))))
                (new-pos (+ start (length new-text))))
            (make-lg-buffer--state
             :text replaced
             :point new-pos
             :mark new-pos)))))))

(defconst lg-buffer-point
  (lg-ref-lens lg-buffer--point-o)
  "Lens focusing point position in a buffer.")

(defconst lg-buffer-state-point
  lg-buffer--point-o
  "Pure lens focusing point position in `lg-buffer--state'.")

(defconst lg-buffer-mark
  (lg-ref-lens lg-buffer--mark-o)
  "Lens focusing mark position in a buffer.")

(defconst lg-buffer-state-mark
  lg-buffer--mark-o
  "Pure lens focusing mark position in `lg-buffer--state'.")

(defconst lg-buffer-string
  (lg-ref-lens lg-buffer--string-o)
  "Lens focusing full text contents of a buffer.")

(defconst lg-buffer-state-string
  lg-buffer--string-o
  "Pure lens focusing full text contents in `lg-buffer--state'.")

(defconst lg-buffer-region-string
  (lg-ref-affine lg-buffer--region-string-o)
  "Affine traversal focusing active region text in a buffer.")

(defconst lg-buffer-state-region-string
  lg-buffer--region-string-o
  "Pure affine optic focusing active region text in `lg-buffer--state'.")

(defun lg-buffer-substring (start end)
  "Lens focusing text in fixed START/END buffer positions."
  (let ((state-substring
         (lg-lens
          (lambda (state)
            (substring (lg-buffer--state-text state) (1- start) (1- end)))
          (lambda (state new-text)
            (let* ((text (lg-buffer--state-text state))
                   (replaced (concat (substring text 0 (1- start))
                                     new-text
                                     (substring text (1- end)))))
              (make-lg-buffer--state
               :text replaced
               :point (lg-buffer--clamp-position (lg-buffer--state-point state) replaced)
               :mark (let ((mark-value (lg-buffer--state-mark state)))
                       (when mark-value
                         (lg-buffer--clamp-position mark-value replaced)))))))))
    (lg-ref-lens state-substring)))

(provide 'looking-glass-buffer)

;;; looking-glass-buffer.el ends here
