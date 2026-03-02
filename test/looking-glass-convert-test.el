;;; looking-glass-convert-test.el --- Conversion package tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-convert)

(ert-deftest lg-convert-list-vector-iso ()
  (let ((optic lg-list-vector-iso))
    (should (equal (lg-view optic '(1 2 3)) [1 2 3]))
    (should (equal (lg-review optic [1 2 3]) '(1 2 3)))
    (should (equal (lg-over optic (lambda (v) (vconcat v [4])) '(1 2 3)) '(1 2 3 4)))))

(ert-deftest lg-convert-number-string-prism ()
  (let ((optic lg-number-string-prism))
    (should (equal (lg-preview optic "42") (lg-just 42)))
    (should (equal (lg-preview optic "  -3.5 ") (lg-just -3.5)))
    (should-not (lg-has optic "42x"))
    (should (equal (lg-review optic 15) "15"))))

(ert-deftest lg-convert-char-string-prism ()
  (let ((optic lg-char-string-prism))
    (should (equal (lg-preview optic "A") (lg-just ?A)))
    (should-not (lg-has optic "AB"))
    (should (equal (lg-review optic ?Z) "Z"))))

(ert-deftest lg-convert-symbol-string-prism ()
  (let* ((existing (intern "lg-symbol-string-prism-test"))
         (missing-name (format "lg-unknown-%s" (float-time)))
         (optic lg-symbol-string-prism))
    (should (equal (lg-preview optic "lg-symbol-string-prism-test") (lg-just existing)))
    (should-not (lg-has optic missing-name))
    (should (equal (lg-review optic 'hello) "hello"))))

(ert-deftest lg-convert-alist-plist-prism ()
  (let ((optic lg-alist-plist-prism))
    (should (equal (lg-preview optic '((:a . 1) (:b . nil))) (lg-just '(:a 1 :b nil))))
    (should (equal (lg-review optic '(:a 1 :b nil)) '((:a . 1) (:b))))
    (should-not (lg-has optic '((:a . 1) (:a . 2))))
    (should-error (lg-review optic '(:a 1 :a 2)))))

(provide 'looking-glass-convert-test)

;;; looking-glass-convert-test.el ends here
