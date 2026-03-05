;;; looking-glass-syntax-treesit-test.el --- Tests for tree-sitter syntax adapter -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'looking-glass)
(require 'looking-glass-syntax)
(require 'looking-glass-syntax-treesit)

(cl-defstruct lg-syntax-treesit-test-node
  kind
  start
  end
  text
  parent
  children)

(defun lg-syntax-treesit-test--parse-buffer (buffer)
  "Parse BUFFER into a synthetic root and word children." 
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((root (make-lg-syntax-treesit-test-node
                    :kind 'program
                    :start (point-min)
                    :end (point-max)
                    :text (buffer-string)))
             (children nil))
        (while (re-search-forward "[[:alpha:]_][[:alnum:]_]*" nil t)
          (let* ((start (match-beginning 0))
                 (end (match-end 0))
                 (text (match-string-no-properties 0))
                 (child (make-lg-syntax-treesit-test-node
                         :kind 'identifier
                         :start start
                         :end end
                         :text text
                         :parent root)))
            (push child children)))
        (setf (lg-syntax-treesit-test-node-children root)
              (nreverse children))
        root))))

(defun lg-syntax-treesit-test--node-at-point (buffer point)
  "Return parsed node at POINT in BUFFER." 
  (let* ((root (lg-syntax-treesit-test--parse-buffer buffer))
         (children (lg-syntax-treesit-test-node-children root))
         (match (cl-find-if (lambda (child)
                              (and (>= point (lg-syntax-treesit-test-node-start child))
                                   (< point (lg-syntax-treesit-test-node-end child))))
                            children)))
    (or match root)))

(ert-deftest lg-syntax-treesit-adapter-resolves-from-parser-list ()
  (with-temp-buffer
    (setq-local major-mode 'python-ts-mode)
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
              ((symbol-function 'treesit-parser-list) (lambda () '(parser-1)))
              ((symbol-function 'treesit-parser-language) (lambda (_parser) 'python)))
      (let ((adapter (lg-syntax-adapter-for-buffer (current-buffer))))
        (should (lg-syntax-treesit-adapter-p adapter))
        (should (eq (lg-syntax-treesit-adapter-language adapter) 'python))))))

(ert-deftest lg-syntax-treesit-optics-read-node-metadata ()
  (let* ((root (make-lg-syntax-treesit-test-node :kind 'program :start 1 :end 11 :text "alpha beta"))
         (alpha (make-lg-syntax-treesit-test-node :kind 'identifier :start 1 :end 6 :text "alpha" :parent root))
         (beta (make-lg-syntax-treesit-test-node :kind 'identifier :start 7 :end 11 :text "beta" :parent root))
         (adapter (make-lg-syntax-treesit-adapter :language 'python)))
    (setf (lg-syntax-treesit-test-node-children root) (list alpha beta))
    (with-temp-buffer
      (insert "alpha beta")
      (setq-local major-mode 'python-ts-mode)
      (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
                ((symbol-function 'treesit-node-at)
                 (lambda (point &optional _language)
                   (if (< point 7) alpha beta)))
                ((symbol-function 'treesit-node-parent)
                 (lambda (node) (lg-syntax-treesit-test-node-parent node)))
                ((symbol-function 'treesit-node-child-count)
                 (lambda (node) (length (lg-syntax-treesit-test-node-children node))))
                ((symbol-function 'treesit-node-child)
                 (lambda (node index &optional _named)
                   (nth index (lg-syntax-treesit-test-node-children node))))
                ((symbol-function 'treesit-node-start)
                 (lambda (node) (lg-syntax-treesit-test-node-start node)))
                ((symbol-function 'treesit-node-end)
                 (lambda (node) (lg-syntax-treesit-test-node-end node)))
                ((symbol-function 'treesit-node-type)
                 (lambda (node) (lg-syntax-treesit-test-node-kind node)))
                ((symbol-function 'treesit-node-text)
                 (lambda (node &optional _property)
                   (lg-syntax-treesit-test-node-text node))))
        (let* ((buffer (current-buffer))
               (ref (lg-view! (lg-syntax-node-ref-at-point 2 adapter) buffer))
               (parent (lg-view! lg-syntax-node-ref-parent ref)))
          (should (equal (lg-view! lg-syntax-node-kind ref) 'identifier))
          (should (equal (lg-view! lg-syntax-node-bounds ref) '(1 . 6)))
          (should (equal (lg-view! lg-syntax-node-text ref) "alpha"))
          (should (equal (mapcar (lambda (child)
                                    (lg-view! lg-syntax-node-text child))
                                  (lg-view! lg-syntax-node-children-list parent))
                          '("alpha" "beta"))))))))

(ert-deftest lg-syntax-treesit-set-children-via-slice-updates-buffer ()
  (let ((adapter (make-lg-syntax-treesit-adapter :language 'python)))
    (with-temp-buffer
      (insert "alpha beta")
      (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
                ((symbol-function 'treesit-node-at)
                 (lambda (point &optional _language)
                   (lg-syntax-treesit-test--node-at-point (current-buffer) point)))
                ((symbol-function 'treesit-node-parent)
                 (lambda (node) (lg-syntax-treesit-test-node-parent node)))
                ((symbol-function 'treesit-node-child-count)
                 (lambda (node)
                   (if (eq (lg-syntax-treesit-test-node-kind node) 'program)
                       (length (lg-syntax-treesit-test-node-children
                                (lg-syntax-treesit-test--parse-buffer (current-buffer))))
                     0)))
                ((symbol-function 'treesit-node-child)
                 (lambda (node index &optional _named)
                   (if (eq (lg-syntax-treesit-test-node-kind node) 'program)
                       (nth index
                            (lg-syntax-treesit-test-node-children
                             (lg-syntax-treesit-test--parse-buffer (current-buffer))))
                     nil)))
                ((symbol-function 'treesit-node-start)
                 (lambda (node) (lg-syntax-treesit-test-node-start node)))
                ((symbol-function 'treesit-node-end)
                 (lambda (node) (lg-syntax-treesit-test-node-end node)))
                ((symbol-function 'treesit-node-type)
                 (lambda (node) (lg-syntax-treesit-test-node-kind node)))
                ((symbol-function 'treesit-node-text)
                 (lambda (node &optional _property)
                   (lg-syntax-treesit-test-node-text node))))
        (let* ((buffer (current-buffer))
               (children-optic
                (lg-compose (lg-syntax-node-ref-at-point 2 adapter)
                            lg-syntax-node-ref-parent
                            lg-syntax-node-children-list))
               (children (lg-view! children-optic buffer))
               (beta (cadr children)))
          (lg-set! (lg-compose children-optic (lg-slice 1 1))
                   (list beta)
                   buffer)
          (should (equal (buffer-string) "alpha beta beta"))
          (should (equal (mapcar (lambda (child)
                                   (lg-view! lg-syntax-node-text child))
                                 (lg-view! children-optic buffer))
                         '("alpha" "beta" "beta"))))))))

(ert-deftest lg-syntax-treesit-set-children-inserts-into-empty-delimited-node ()
  (let ((adapter (make-lg-syntax-treesit-adapter :language 'python)))
    (with-temp-buffer
      (insert "()")
      (cl-letf (((symbol-function 'treesit-available-p) (lambda () t))
                ((symbol-function 'treesit-node-at)
                 (lambda (point &optional _language)
                   (lg-syntax-treesit-test--node-at-point (current-buffer) point)))
                ((symbol-function 'treesit-node-parent)
                 (lambda (node) (lg-syntax-treesit-test-node-parent node)))
                ((symbol-function 'treesit-node-child-count)
                 (lambda (node)
                   (if (eq (lg-syntax-treesit-test-node-kind node) 'program)
                       (length (lg-syntax-treesit-test-node-children
                                (lg-syntax-treesit-test--parse-buffer (current-buffer))))
                     0)))
                ((symbol-function 'treesit-node-child)
                 (lambda (node index &optional _named)
                   (if (eq (lg-syntax-treesit-test-node-kind node) 'program)
                       (nth index
                            (lg-syntax-treesit-test-node-children
                             (lg-syntax-treesit-test--parse-buffer (current-buffer))))
                     nil)))
                ((symbol-function 'treesit-node-start)
                 (lambda (node) (lg-syntax-treesit-test-node-start node)))
                ((symbol-function 'treesit-node-end)
                 (lambda (node) (lg-syntax-treesit-test-node-end node)))
                ((symbol-function 'treesit-node-type)
                 (lambda (node) (lg-syntax-treesit-test-node-kind node)))
                ((symbol-function 'treesit-node-text)
                 (lambda (node &optional _property)
                   (lg-syntax-treesit-test-node-text node))))
        (let* ((buffer (current-buffer))
               (child-node (make-lg-syntax-treesit-test-node
                            :kind 'identifier
                            :start 2
                            :end 3
                            :text "item"))
               (child-ref (make-lg-syntax-ref :buffer buffer
                                              :adapter adapter
                                              :node child-node)))
          (lg-set! (lg-compose (lg-syntax-node-ref-at-point 1 adapter)
                               lg-syntax-node-children-list)
                   (list child-ref)
                   buffer)
          (should (equal (buffer-string) "(item)"))
          (should (equal (mapcar (lambda (child)
                                   (lg-view! lg-syntax-node-text child))
                                 (lg-view! (lg-compose (lg-syntax-node-ref-at-point 1 adapter)
                                                       lg-syntax-node-children-list)
                                           buffer))
                         '("item"))))))))

(provide 'looking-glass-syntax-treesit-test)

;;; looking-glass-syntax-treesit-test.el ends here
