;;; looking-glass-tree-sitter-test.el --- Tests for looking-glass-tree-sitter -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'looking-glass)
(require 'looking-glass-buffer)
(require 'looking-glass-tree-sitter)

(ert-deftest lg-ts-query-rewrites-captured-spans ()
  (with-temp-buffer
    (insert "x 10 y 20 z")
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'lg-ts--captures-for-query)
                 (lambda (_buffer _query _language _capture)
                   (list (list :capture 'num :node 'n1 :start 3 :end 5 :text "10" :index '(num . 0))
                         (list :capture 'num :node 'n2 :start 8 :end 10 :text "20" :index '(num . 1))))))
        (should (eq (lg-over (lg-ts-query 'ignored)
                             (lambda (s)
                               (number-to-string (1+ (string-to-number s))))
                             buf)
                    buf))
        (should (equal (buffer-string) "x 11 y 21 z"))))))

(ert-deftest lg-ts-indexed-query-collects-capture-indices ()
  (with-temp-buffer
    (insert "x 10 y 20")
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'lg-ts--captures-for-query)
                 (lambda (_buffer _query _language _capture)
                   (list (list :capture 'num :node 'n1 :start 3 :end 5 :text "10" :index '(num . 0))
                         (list :capture 'num :node 'n2 :start 8 :end 10 :text "20" :index '(num . 1))))))
        (should (equal (lg-ito-list-of (lg-its-query 'ignored) buf)
                       '(((num . 0) . "10")
                         ((num . 1) . "20"))))))))

(ert-deftest lg-ts-query-node-folds-expose-captured-nodes ()
  (with-temp-buffer
    (insert "irrelevant")
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'lg-ts--captures-for-query)
                 (lambda (_buffer _query _language _capture)
                   (list (list :capture 'lhs :node 'node-a :start 1 :end 2 :text "i" :index '(lhs . 0))
                         (list :capture 'rhs :node 'node-b :start 2 :end 3 :text "r" :index '(rhs . 1))))))
        (should (equal (lg-to-list-of (lg-ts-query-nodes 'ignored) buf)
                       '(node-a node-b)))
        (should (equal (lg-ito-list-of (lg-its-query-nodes 'ignored) buf)
                       '(((lhs . 0) . node-a)
                         ((rhs . 1) . node-b))))))))

(ert-deftest lg-ts-integration-json-query-increments-numbers ()
  (skip-unless (and (fboundp 'json-ts-mode)
                    (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'json)))
  (with-temp-buffer
    (json-ts-mode)
    (insert "{\"a\":1,\"b\":20}")
    (let ((buf (current-buffer)))
      (lg-over (lg-ts-query '((number) @num) 'json 'num)
               (lambda (s)
                 (number-to-string (1+ (string-to-number s))))
               buf)
      (should (equal (buffer-string) "{\"a\":2,\"b\":21}")))))

(provide 'looking-glass-tree-sitter-test)

;;; looking-glass-tree-sitter-test.el ends here
