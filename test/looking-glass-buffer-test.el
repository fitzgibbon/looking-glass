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

(provide 'looking-glass-buffer-test)

;;; looking-glass-buffer-test.el ends here
