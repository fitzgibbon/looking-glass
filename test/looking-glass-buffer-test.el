;;; looking-glass-buffer-test.el --- Tests for looking-glass-buffer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'subr-x)
(require 'looking-glass)
(require 'looking-glass-buffer)

(ert-deftest lg-buffer-string-view-set-respects-narrowing ()
  (with-temp-buffer
    (insert "head\nbody\ntail")
    (let ((buf (current-buffer)))
      (save-restriction
        (narrow-to-region 6 10)
        (should (equal (lg-view (lg-buffer-string) buf) "body"))
        (should (eq (lg-set (lg-buffer-string) "BODY" buf) buf)))
      (should (equal (buffer-string) "head\nBODY\ntail")))))

(ert-deftest lg-buffer-region-lens-updates-fixed-span ()
  (with-temp-buffer
    (insert "abcdef")
    (let ((buf (current-buffer)))
      (should (equal (lg-view (lg-buffer-region 2 5) buf) "bcd"))
      (should (eq (lg-over (lg-buffer-region 2 5) #'upcase buf) buf))
      (should (equal (buffer-string) "aBCDef")))))

(ert-deftest lg-buffer-lines-traversal-roundtrips-line-breaks ()
  (with-temp-buffer
    (insert "a\nb\nc")
    (let ((buf (current-buffer)))
      (should (equal (lg-to-list-of (lg-buffer-lines) buf)
                     '("a\n" "b\n" "c")))
      (should (eq (lg-over (lg-buffer-lines) #'upcase buf) buf))
      (should (equal (buffer-string) "A\nB\nC")))))

(ert-deftest lg-ibuffer-lines-reports-zero-based-indices ()
  (with-temp-buffer
    (insert "x\ny\n")
    (let ((buf (current-buffer)))
      (should (equal (lg-ito-list-of (lg-ibuffer-lines) buf)
                     '((0 . "x\n") (1 . "y\n"))))
      (should (eq (lg-iover (lg-ibuffer-lines)
                            (lambda (idx line)
                              (if (= idx 1) (upcase line) line))
                            buf)
                  buf))
      (should (equal (buffer-string) "x\nY\n")))))

(ert-deftest lg-buffer-matches-and-indexed-matches-compose-with-regex ()
  (with-temp-buffer
    (insert "x=10,y=20")
    (let ((buf (current-buffer)))
      (should (equal (lg-to-list-of (lg-buffer-matches "[0-9]+") buf)
                     '("10" "20")))
      (should (equal (lg-ito-list-of (lg-ibuffer-matches "[0-9]+") buf)
                     '((0 . "10") (1 . "20"))))
      (should (eq (lg-over (lg-buffer-matches "[0-9]+")
                           (lambda (n) (number-to-string (* 2 (string-to-number n))))
                           buf)
                  buf))
      (should (equal (buffer-string) "x=20,y=40")))))

(ert-deftest lg-buffer-metadata-lenses-name-modified-point-mark ()
  (with-temp-buffer
    (insert "hello")
    (let ((buf (current-buffer)))
      (should (stringp (lg-view (lg-buffer-name) buf)))
      (should (eq (lg-set (lg-buffer-name) "lg-buffer-meta" buf) buf))
      (should (string-prefix-p "lg-buffer-meta" (buffer-name buf)))

      (set-buffer-modified-p nil)
      (should (eq (lg-set (lg-buffer-modified-p) t buf) buf))
      (should (buffer-modified-p buf))

      (goto-char 2)
      (should (= (lg-view (lg-buffer-point) buf) 2))
      (should (eq (lg-set (lg-buffer-point) 5 buf) buf))
      (should (= (point) 5))

      (set-marker (mark-marker) nil)
      (should (null (lg-view (lg-buffer-mark) buf)))
      (should (eq (lg-set (lg-buffer-mark) 3 buf) buf))
      (should (= (mark t) 3))
      (should (eq (lg-set (lg-buffer-mark) nil buf) buf))
      (should (null (mark t))))))

(ert-deftest lg-buffer-content-edits-preserve-point-and-mark ()
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((buf (current-buffer)))
      (goto-char 3)
      (set-mark 8)
      (activate-mark)
      (let ((point-before (point))
            (mark-before (mark t)))
        (lg-set (lg-buffer-string) "LINE1\nline2" buf)
        (should (= (point) point-before))
        (should (= (mark t) mark-before))))))

(ert-deftest lg-buffer-write-signals-on-read-only ()
  (with-temp-buffer
    (insert "abc")
    (let ((buf (current-buffer)))
      (setq buffer-read-only t)
      (should-error (lg-set (lg-buffer-string) "xyz" buf)))))

(provide 'looking-glass-buffer-test)

;;; looking-glass-buffer-test.el ends here
