;;; looking-glass-buffer-test.el --- Buffer package tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-buffer)

(ert-deftest lg-buffer-optics ()
  (with-temp-buffer
    (insert "hello world")
    (let ((buffer (current-buffer)))
      (should-error (lg-view lg-buffer-point buffer))
      (should (= (lg-view! lg-buffer-point buffer) (point-max)))
      (should-error (lg-set lg-buffer-point 1 buffer))
      (should (eq (lg-set! lg-buffer-point 1 buffer) buffer))
      (should (= (point) 1))
      (should (equal (lg-view! lg-buffer-string buffer) "hello world"))
      (should-error (lg-set lg-buffer-string "updated" buffer))
      (should (eq (lg-set! lg-buffer-string "updated" buffer) buffer))
      (should (equal (buffer-string) "updated"))
      (goto-char 1)
      (set-mark 8)
      (should-error (lg-preview lg-buffer-region-string buffer))
      (should (equal (lg-preview! lg-buffer-region-string buffer) (lg-just "updated")))
      (should-error (lg-set lg-buffer-region-string "text" buffer))
      (should (eq (lg-set! lg-buffer-region-string "text" buffer) buffer))
      (should (equal (buffer-string) "text")))))

(ert-deftest lg-buffer-state-actions ()
  (with-temp-buffer
    (insert "hello")
    (let ((buffer (current-buffer)))
      (lg-over! lg-buffer-string (lambda (_text) "updated") buffer)
      (should (equal (buffer-string) "updated"))
      (lg-set! lg-buffer-point 1 buffer)
      (should (= (point) 1))
      (let ((viewed (lg-view! lg-buffer-string buffer)))
        (should (equal viewed "updated"))))))

(ert-deftest lg-buffer-state-monad-rejects-impure-optics ()
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (set-mark 6)
    (let* ((buffer (current-buffer))
           (program nil)
           (result-state nil)
           (result nil)
           (final-buffer nil))
      (setq program
            (lg-state-bind
             (lg-state-view-of lg-buffer-string)
             (lambda (before)
               (lg-state-bind
                (lg-state-over-of lg-buffer-region-string #'upcase)
                (lambda (_ignored)
                  (lg-state-bind
                   (lg-state-view-of lg-buffer-string)
                   (lambda (after)
                     (lg-state-pure (cons before after)))))))))
      (should-error (lg-run-state program buffer))
      (should (equal (buffer-string) "hello world")))))

(ert-deftest lg-buffer-ref-bang-helpers ()
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char 1)
    (set-mark 6)
    (let ((buffer (current-buffer)))
      (should (eq (lg-over! lg-buffer-region-string #'upcase buffer) buffer))
      (should (equal (buffer-string) "ALPHA beta"))
      (should (eq (lg-set! lg-buffer-point 3 buffer) buffer))
      (should (= (point) 3)))))

(ert-deftest lg-buffer-effect-programs ()
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (set-mark 6)
    (let* ((buffer (current-buffer))
           (program nil)
           (result nil))
      (setq program
            (lg-effect-bind
             (lg-effect-view-of lg-buffer-string)
             (lambda (before)
               (lg-effect-bind
                (lg-effect-over-of lg-buffer-region-string #'upcase)
                (lambda (_)
                  (lg-effect-map
                   (lambda (after)
                     (cons before after))
                   (lg-effect-view-of lg-buffer-string)))))))
      (setq result (lg-run-effect program buffer))
      (should (equal result '("hello world" . "HELLO world")))
      (should (equal (buffer-string) "HELLO world")))))

(ert-deftest lg-buffer-effect-operations-dispatch ()
  (with-temp-buffer
    (insert "abc")
    (let ((buffer (current-buffer)))
      (should (= (lg-run-effect
                  (lg-effect-perform (make-lg-buffer-op-point-get))
                  buffer)
                 (point-max)))
      (should (eq (lg-run-effect
                   (lg-effect-perform (make-lg-buffer-op-string-set :text "xyz"))
                   buffer)
                  buffer))
      (should (equal (buffer-string) "xyz")))))

(provide 'looking-glass-buffer-test)

;;; looking-glass-buffer-test.el ends here
