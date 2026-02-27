;;; looking-glass-test.el --- Tests for looking-glass -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'looking-glass)

(ert-deftest lg-lens-basic-view-set-over ()
  (let ((optic (lg-nth 1)))
    (should (equal (lg-view optic '(10 20 30)) 20))
    (should (equal (lg-set optic 99 '(10 20 30)) '(10 99 30)))
    (should (equal (lg-over optic (lambda (v) (+ v 1)) '(10 20 30))
                   '(10 21 30)))))

(ert-deftest lg-compose-lens-traversal ()
  (let* ((optic (lg-compose (lg-nth 1) (lg-each-list)))
         (source '((a b) (1 2 3) (x y))))
    (should (equal (lg-to-list-of optic source) '(1 2 3)))
    (should (equal (lg-over optic (lambda (n) (* n 10)) source)
                   '((a b) (10 20 30) (x y))))))

(ert-deftest lg-prism-preview-review ()
  (let ((optic (lg-just)))
    (should (equal (lg-preview optic 42) 42))
    (should (equal (lg-preview optic nil) nil))
    (should (equal (lg-review optic "ok") "ok"))))

(ert-deftest lg-regex-over-full-match ()
  (let ((optic (lg-regex "[0-9]+")))
    (should (equal (lg-to-list-of optic "a12-b34") '("12" "34")))
    (should (equal (lg-over optic (lambda (n) (format "[%s]" n)) "a12-b34")
                   "a[12]-b[34]"))))

(ert-deftest lg-regex-over-capture-group ()
  (let ((optic (lg-regex "\\([a-z]+\\)=\\([0-9]+\\)" 2)))
    (should (equal (lg-to-list-of optic "x=10,y=20") '("10" "20")))
    (should (equal (lg-over optic (lambda (n) (number-to-string (* 2 (string-to-number n))))
                           "x=10,y=20")
                   "x=20,y=40"))))

(ert-deftest lg-view-errors-on-non-single-focus ()
  (should-error (lg-view (lg-regex "[a-z]") "abc")))

(provide 'looking-glass-test)

;;; looking-glass-test.el ends here
