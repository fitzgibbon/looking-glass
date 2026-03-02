;;; looking-glass-test.el --- Tests for looking-glass -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)

(ert-deftest lg-lens-view-set-over ()
  (let ((optic (lg-car-lens)))
    (should (equal (lg-view optic '(1 . 2)) 1))
    (should (equal (lg-set optic 9 '(1 . 2)) '(9 . 2)))
    (should (equal (lg-over optic (lambda (x) (+ x 3)) '(1 . 2)) '(4 . 2)))))

(ert-deftest lg-compose-nested-pairs ()
  (let* ((nested '((1 . 2) . 3))
         (optic (lg-compose (lg-cdr-lens) (lg-car-lens))))
    (should (equal (lg-view optic nested) 2))
    (should (equal (lg-set optic 7 nested) '((1 . 7) . 3)))))

(ert-deftest lg-traversal-list-over-and-fold ()
  (let ((optic (lg-list-traversal)))
    (should (equal (lg-over optic (lambda (x) (* x 10)) '(1 2 3)) '(10 20 30)))
    (should (equal (lg-to-list-of optic '(1 2 3)) '(1 2 3)))))

(ert-deftest lg-prism-non-nil ()
  (let ((optic (lg-non-nil)))
    (should (equal (lg-preview optic 42) 42))
    (should (equal (lg-review optic 42) 42))
    (should (equal (lg-preview-result optic nil) '(nil . nil)))
    (should-not (lg-has optic nil))))

(ert-deftest lg-review-support ()
  (let* ((just (lg-just))
         (iso (lg-iso #'number-to-string #'string-to-number))
         (composed (lg-compose just iso)))
    (should (equal (lg-review just 7) '(just . 7)))
    (should (equal (lg-review iso "42") 42))
    (should (equal (lg-review composed "9") '(just . 9)))
    (should-error (lg-review (lg-list-traversal) 1))))

(ert-deftest lg-preview-result-disambiguates-present-nil ()
  (let* ((optic (lg-plist-key-traversal :name))
         (present-nil '(:name nil :age 10))
         (missing '(:age 10)))
    (should (equal (lg-preview-result optic present-nil) '(t . nil)))
    (should (equal (lg-preview-result optic missing) '(nil . nil)))
    (should (lg-has optic present-nil))
    (should-not (lg-has optic missing))))

(ert-deftest lg-view-non-nil-signals ()
  (let ((optic (lg-plist-key-traversal :name)))
    (should-error (lg-view-non-nil optic '(:name nil)) :type 'lg-expected-non-nil)
    (should (equal (lg-view-non-nil optic '(:name "Ada")) "Ada"))))

(ert-deftest lg-alist-key-traversal ()
  (let ((optic (lg-alist-key-traversal "x" #'string=)))
    (should (equal (lg-preview-result optic '(("x" . nil) ("y" . 2))) '(t . nil)))
    (should (equal (lg-over optic (lambda (_v) 9) '(("x" . nil) ("y" . 2)))
                   '(("x" . 9) ("y" . 2))))))

(ert-deftest lg-vector-and-string-traversals ()
  (should (equal (lg-over (lg-vector-traversal) (lambda (x) (+ x 1)) [1 2 3]) [2 3 4]))
  (should (equal (lg-over (lg-string-traversal) #'upcase "abc") "ABC"))
  (should (equal (lg-to-list-of (lg-string-traversal) "ab") '("a" "b"))))

(ert-deftest lg-indexed-list-base-and-ops ()
  (let ((optic (lg-indexed-list-traversal)))
    (should (equal (lg-ito-list-of optic '(10 nil 30))
                   '((0 . 10) (1) (2 . 30))))
    (should (equal (lg-ipreview-result optic '(10 nil 30)) '(t . (0 . 10))))
    (should (equal (lg-ipreview-result optic '()) '(nil . nil)))
    (should (equal (lg-iover optic (lambda (i x) (if (= i 1) 99 x)) '(10 20 30))
                   '(10 99 30)))))

(ert-deftest lg-indexed-reindexing ()
  (let* ((base (lg-indexed-list-traversal))
         (shifted (lg-ireindexed (lambda (index) (+ index 10)) base)))
    (should (equal (lg-ito-list-of shifted '(4 5)) '((10 . 4) (11 . 5))))
    (should (equal (lg-iover shifted (lambda (i x) (+ i x)) '(4 5)) '(14 16)))))

(ert-deftest lg-extension-point-custom-affine-optic ()
  (let ((optic
         (lg-traversal
          (lambda (afb source applicative)
            (let ((pure (lg-applicative-pure applicative))
                  (fmap (lg-applicative-fmap applicative)))
              (if (numberp source)
                  (funcall fmap (lambda (v) (* v 2)) (funcall afb source))
                (funcall pure source)))))))
    (should (equal (lg-over optic (lambda (x) (+ x 1)) 10) 22))
    (should (equal (lg-over optic (lambda (x) (+ x 1)) 'nope) 'nope))
    (should (equal (lg-preview-result optic 10) '(t . 10)))
    (should (equal (lg-preview-result optic 'nope) '(nil . nil)))))

;;; looking-glass-test.el ends here
