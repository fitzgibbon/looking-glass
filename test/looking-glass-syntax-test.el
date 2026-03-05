;;; looking-glass-syntax-test.el --- Tests for syntax optics -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-syntax)

(cl-defstruct lg-syntax-test-adapter)

(defun lg-syntax-test--node (id start end)
  "Build test node with ID and START/END bounds."
  (list :id id :start start :end end))

(defun lg-syntax-test--node-id (node)
  "Return NODE id."
  (plist-get node :id))

(defun lg-syntax-test--node-start (node)
  "Return NODE start position."
  (plist-get node :start))

(defun lg-syntax-test--node-end (node)
  "Return NODE end position."
  (plist-get node :end))

(cl-defmethod lg-syntax-adapter-for-mode ((_mode (eql lg-syntax-test-mode)))
  (make-lg-syntax-test-adapter))

(cl-defmethod lg-syntax-node-at-point ((_adapter lg-syntax-test-adapter)
                                       (_buffer buffer)
                                       point)
  (cond
   ((and (>= point 1) (<= point 6))
    (lg-syntax-test--node 'alpha 1 6))
   ((and (>= point 7) (<= point 11))
    (lg-syntax-test--node 'beta 7 11))
   (t nil)))

(cl-defmethod lg-syntax-node-parent ((_adapter lg-syntax-test-adapter)
                                     (_buffer buffer)
                                     node)
  (when (memq (lg-syntax-test--node-id node) '(alpha beta))
    (lg-syntax-test--node 'root 1 11)))

(cl-defmethod lg-syntax-node-children ((_adapter lg-syntax-test-adapter)
                                       (_buffer buffer)
                                       node)
  (if (eq (lg-syntax-test--node-id node) 'root)
      (list (lg-syntax-test--node 'alpha 1 6)
            (lg-syntax-test--node 'beta 7 11))
    nil))

(cl-defmethod lg-syntax-node-bounds-of ((_adapter lg-syntax-test-adapter)
                                        (_buffer buffer)
                                        node)
  (cons (lg-syntax-test--node-start node)
        (lg-syntax-test--node-end node)))

(cl-defmethod lg-syntax-node-kind-of ((_adapter lg-syntax-test-adapter)
                                      (_buffer buffer)
                                      node)
  (lg-syntax-test--node-id node))

(ert-deftest lg-syntax-node-ref-resolves-adapter-by-major-mode ()
  (with-temp-buffer
    (insert "alpha beta")
    (setq-local major-mode 'lg-syntax-test-mode)
    (goto-char 2)
    (let* ((buffer (current-buffer))
           (ref (lg-view (lg-syntax-node-ref-at-point) buffer)))
      (should (equal (lg-view lg-syntax-node-kind ref) 'alpha))
      (should (equal (lg-view lg-syntax-node-bounds ref) '(1 . 6)))
      (should (equal (lg-view lg-syntax-node-text ref) "alpha")))))

(ert-deftest lg-syntax-node-text-lens-edits-buffer-through-effects ()
  (with-temp-buffer
    (insert "alpha beta")
    (setq-local major-mode 'lg-syntax-test-mode)
    (let* ((buffer (current-buffer))
           (optic (lg-compose (lg-syntax-node-ref-at-point 2)
                              lg-syntax-node-text)))
      (should (eq (lg-set optic "ALPHA" buffer) buffer))
      (should (equal (buffer-string) "ALPHA beta"))
      (should (equal (lg-view optic buffer) "ALPHA")))))

(ert-deftest lg-syntax-node-parent-and-children-navigation ()
  (with-temp-buffer
    (insert "alpha beta")
    (setq-local major-mode 'lg-syntax-test-mode)
    (let* ((buffer (current-buffer))
           (leaf (lg-view (lg-syntax-node-ref-at-point 2) buffer))
           (root (lg-view lg-syntax-node-ref-parent leaf))
           (children (lg-to-list-of lg-syntax-node-ref-children root)))
      (should (equal (lg-view lg-syntax-node-kind root) 'root))
      (should (equal (mapcar (lambda (child)
                               (lg-view lg-syntax-node-kind child))
                             children)
                     '(alpha beta))))))

(ert-deftest lg-syntax-node-ref-missing-adapter-returns-nothing ()
  (with-temp-buffer
    (insert "alpha beta")
    (let ((buffer (current-buffer)))
      (should (equal (lg-preview (lg-syntax-node-ref-at-point 2) buffer)
                     lg-nothing)))))

(provide 'looking-glass-syntax-test)

;;; looking-glass-syntax-test.el ends here
