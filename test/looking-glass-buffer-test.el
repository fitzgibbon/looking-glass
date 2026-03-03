;;; looking-glass-buffer-test.el --- Buffer package tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-buffer)

(ert-deftest lg-buffer-optics ()
  (with-temp-buffer
    (insert "hello world")
    (let ((buffer (current-buffer)))
      (should (= (lg-view lg-buffer-point buffer) (point-max)))
      (should (eq (lg-set lg-buffer-point 1 buffer) buffer))
      (should (= (point) 1))
      (should (equal (lg-view lg-buffer-string buffer) "hello world"))
      (should (eq (lg-set lg-buffer-string "updated" buffer) buffer))
      (should (equal (buffer-string) "updated"))
      (goto-char 1)
      (set-mark 8)
      (should (equal (lg-preview lg-buffer-region-string buffer) (lg-just "updated")))
      (should (eq (lg-set lg-buffer-region-string "text" buffer) buffer))
      (should (equal (buffer-string) "text")))))

(ert-deftest lg-buffer-state-actions ()
  (with-temp-buffer
    (insert "hello")
    (let ((buffer (current-buffer)))
      (lg-run-state-on-ref! (lg-state-set-of lg-buffer-state-string "updated") buffer)
      (should (equal (buffer-string) "updated"))
      (lg-run-state-on-ref! (lg-state-set-of lg-buffer-state-point 1) buffer)
      (should (= (point) 1))
      (let ((viewed (lg-run-state-on-ref! (lg-state-view-of lg-buffer-state-string) buffer)))
        (should (equal viewed "updated"))))))

(ert-deftest lg-buffer-reversible-edit ()
  (with-temp-buffer
    (insert "hello")
    (let* ((buffer (current-buffer))
           (undo (lg-ref-over-edit! lg-buffer-state-string
                                    (lambda (text)
                                      (concat text " world"))
                                    buffer)))
      (should (equal (buffer-string) "hello world"))
      (funcall undo)
      (should (equal (buffer-string) "hello")))))

(ert-deftest lg-buffer-state-monad-sequencing ()
  (with-temp-buffer
    (insert "hello world")
    (goto-char 1)
    (set-mark 6)
    (let* ((buffer (current-buffer))
           (program
            (lg-state-bind
             (lg-state-view-of lg-buffer-state-string)
             (lambda (before)
               (lg-state-bind
                (lg-state-over-of lg-buffer-state-region-string #'upcase)
                (lambda (_ignored)
                  (lg-state-bind
                   (lg-state-view-of lg-buffer-state-string)
                   (lambda (after)
                     (lg-state-pure (cons before after)))))))))
           (result (lg-run-state-on-ref! program buffer)))
      (should (equal result '("hello world" . "HELLO world")))
      (should (equal (buffer-string) "HELLO world")))))

(ert-deftest lg-buffer-ref-bang-helpers ()
  (with-temp-buffer
    (insert "alpha beta")
    (goto-char 1)
    (set-mark 6)
    (let ((buffer (current-buffer)))
      (should (eq (lg-over! lg-buffer-state-region-string #'upcase buffer) buffer))
      (should (equal (buffer-string) "ALPHA beta"))
      (should (eq (lg-set! lg-buffer-state-point 3 buffer) buffer))
      (should (= (point) 3)))))

(provide 'looking-glass-buffer-test)

;;; looking-glass-buffer-test.el ends here
