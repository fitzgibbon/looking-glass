;;; looking-glass-test.el --- Tests for looking-glass -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'looking-glass)

(ert-deftest lg-lens-basic-view-set-over ()
  (let ((optic (lg-nth 1)))
    (should (equal (lg-kind optic) 'lens))
    (should (equal (lg-view optic '(10 20 30)) 20))
    (should (equal (lg-set optic 99 '(10 20 30)) '(10 99 30)))
    (should (equal (lg-over optic (lambda (v) (+ v 1)) '(10 20 30))
                   '(10 21 30)))))

(ert-deftest lg-compose-lens-traversal ()
  (let* ((optic (lg-compose (lg-nth 1) (lg-each-list)))
         (source '((a b) (1 2 3) (x y))))
    (should (equal (lg-kind optic) 'traversal))
    (should (equal (lg-to-list-of optic source) '(1 2 3)))
    (should (equal (lg-over optic (lambda (n) (* n 10)) source)
                   '((a b) (10 20 30) (x y))))))

(ert-deftest lg-prism-preview-review ()
  (let ((optic (lg-just)))
    (should (equal (lg-kind optic) 'prism))
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

(ert-deftest lg-kind-capabilities-inspection ()
  (let ((optic (lg-compose (lg-nth 1) (lg-each-list))))
    (should (equal (lg-kind optic) 'traversal))
    (should (lg-can-p optic 'over))
    (should (lg-can-p optic 'collect))
    (should (not (lg-can-p optic 'review)))))

(ert-deftest lg-profunctor-representation-composes ()
  (let* ((optic (lg-compose (lg-nth 1) (lg-each-list) (lg-just)))
         (rep (lg-optic-rep optic)))
    (should (functionp rep))
    (should (equal (lg-preview optic '((a) (1 2) nil)) 1))))

(ert-deftest lg-indexed-profunctor-representation-present ()
  (let ((optic (lg-ieach-list)))
    (should (functionp (lg-optic-rep optic)))
    (should (functionp (lg-optic-irep optic)))))

(ert-deftest lg-ifiltered-replaces-index-when-guard ()
  (let* ((source '(10 20 30 40 50))
         (pairs (lg-ito-list-of (lg-ieach-list) source))
         (optic (lg-compose (lg-each-list)
                            (lg-ifiltered (lambda (idx _value)
                                            (= 0 (% idx 2)))))))
    (should (equal (lg-to-list-of optic pairs)
                   '((0 . 10) (2 . 30) (4 . 50))))))

(ert-deftest lg-indexed-traversal-collect-and-over ()
  (let ((optic (lg-ieach-list)))
    (should (equal (lg-kind optic) 'indexed-traversal))
    (should (equal (lg-ito-list-of optic '(a b c))
                   '((0 . a) (1 . b) (2 . c))))
    (should (equal (lg-iover optic
                             (lambda (i v)
                               (if (= i 1) (intern (format "%s!" v)) v))
                             '(a b c))
                   '(a b! c)))))

(ert-deftest lg-indexed-compose-propagates-path-index ()
  (let* ((optic (lg-compose (lg-ieach-list) (lg-ieach-list)))
         (source '((a b) (c d))))
    (should (equal (lg-kind optic) 'indexed-traversal))
    (should (equal (lg-ito-list-of optic source)
                   '(((0 . 0) . a) ((0 . 1) . b) ((1 . 0) . c) ((1 . 1) . d))))
    (should (equal (lg-iover optic
                             (lambda (idx v)
                               (if (equal idx '(1 . 0)) 'X v))
                             source)
                   '((a b) (X d))))))

(ert-deftest lg-indexed-compose-mixed-indexed-and-unindexed ()
  (let* ((optic (lg-compose (lg-ieach-list) (lg-each-list)))
         (source '((a b) (c d))))
    (should (equal (lg-kind optic) 'indexed-traversal))
    (should (equal (lg-ito-list-of optic source)
                   '((0 . a) (0 . b) (1 . c) (1 . d))))
    (should (equal (lg-iover optic
                             (lambda (idx v)
                               (if (= idx 0) (intern (format "%s0" v)) v))
                             source)
                   '((a0 b0) (c d))))))

(ert-deftest lg-review-only-optic ()
  (let ((optic (lg-reviewer (lambda (value) (list :wrapped value)))))
    (should (equal (lg-kind optic) 'review))
    (should (equal (lg-review optic 7) '(:wrapped 7)))
    (should-error (lg-over optic #'identity 1))))

(provide 'looking-glass-test)

;;; looking-glass-test.el ends here
