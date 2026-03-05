;;; looking-glass-syntax-test.el --- Tests for syntax optics -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'looking-glass)
(require 'looking-glass-syntax)

(cl-defstruct lg-syntax-test-adapter)

(defun lg-syntax-test--node (kind start end &optional text)
  "Build test node with KIND, START/END bounds, and optional TEXT."
  (list :kind kind :start start :end end :text text))

(defun lg-syntax-test--node-id (node)
  "Return NODE id."
  (plist-get node :kind))

(defun lg-syntax-test--node-start (node)
  "Return NODE start position."
  (plist-get node :start))

(defun lg-syntax-test--node-end (node)
  "Return NODE end position."
  (plist-get node :end))

(defun lg-syntax-test--node-text (node)
  "Return NODE text."
  (or (plist-get node :text)
      (buffer-substring-no-properties
       (lg-syntax-test--node-start node)
       (lg-syntax-test--node-end node))))

(defun lg-syntax-test--root-node (buffer)
  "Return root node for BUFFER."
  (with-current-buffer buffer
    (lg-syntax-test--node 'root (point-min) (point-max) (buffer-string))))

(defun lg-syntax-test--word-nodes (buffer)
  "Return parsed word child nodes for BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (nodes)
        (while (re-search-forward "[^[:space:]]+" nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0))
                (text (match-string-no-properties 0)))
            (push (lg-syntax-test--node (intern text) start end text) nodes)))
        (nreverse nodes)))))

(cl-defmethod lg-syntax-adapter-for-mode ((_mode (eql lg-syntax-test-mode)))
  (make-lg-syntax-test-adapter))

(cl-defmethod lg-syntax-node-at-point ((_adapter lg-syntax-test-adapter)
                                       (buffer buffer)
                                       point)
  (or
   (cl-find-if (lambda (node)
                 (and (>= point (lg-syntax-test--node-start node))
                      (< point (lg-syntax-test--node-end node))))
               (lg-syntax-test--word-nodes buffer))
   (when (with-current-buffer buffer
           (and (>= point (point-min))
                (<= point (point-max))))
     (lg-syntax-test--root-node buffer))))

(cl-defmethod lg-syntax-node-parent ((_adapter lg-syntax-test-adapter)
                                     (buffer buffer)
                                     node)
  (unless (eq (lg-syntax-test--node-id node) 'root)
    (lg-syntax-test--root-node buffer)))

(cl-defmethod lg-syntax-node-children ((_adapter lg-syntax-test-adapter)
                                       (buffer buffer)
                                       node)
  (if (eq (lg-syntax-test--node-id node) 'root)
      (lg-syntax-test--word-nodes buffer)
    nil))

(cl-defmethod lg-syntax-node-set-children ((_adapter lg-syntax-test-adapter)
                                           (buffer buffer)
                                           node
                                           children)
  (unless (eq (lg-syntax-test--node-id node) 'root)
    (error "Only root children are writable in test adapter"))
  (with-current-buffer buffer
    (save-excursion
      (let* ((start (point-min))
             (end (point-max))
             (replacement (mapconcat #'lg-syntax-test--node-text children " ")))
        (goto-char start)
        (delete-region start end)
        (insert replacement))))
  (lg-syntax-test--root-node buffer))

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
           (_ (should-error (lg-view (lg-syntax-node-ref-at-point) buffer)))
           (ref (lg-view! (lg-syntax-node-ref-at-point) buffer)))
      (should (equal (lg-view! lg-syntax-node-kind ref) 'alpha))
      (should (equal (lg-view! lg-syntax-node-bounds ref) '(1 . 6)))
      (should (equal (lg-view! lg-syntax-node-text ref) "alpha")))))

(ert-deftest lg-syntax-node-text-lens-edits-buffer-through-effects ()
  (with-temp-buffer
    (insert "alpha beta")
    (setq-local major-mode 'lg-syntax-test-mode)
    (let* ((buffer (current-buffer))
           (optic (lg-compose (lg-syntax-node-ref-at-point 2)
                              lg-syntax-node-text)))
      (should (eq (lg-set! optic "ALPHA" buffer) buffer))
      (should (equal (buffer-string) "ALPHA beta"))
      (should (equal (lg-view! optic buffer) "ALPHA")))))

(ert-deftest lg-syntax-node-parent-and-children-navigation ()
  (with-temp-buffer
    (insert "alpha beta")
    (setq-local major-mode 'lg-syntax-test-mode)
    (let* ((buffer (current-buffer))
           (leaf (lg-view! (lg-syntax-node-ref-at-point 2) buffer))
           (root (lg-view! lg-syntax-node-ref-parent leaf))
           (children (lg-view! lg-syntax-node-children-list root)))
      (should (equal (lg-view! lg-syntax-node-kind root) 'root))
      (should (equal (mapcar (lambda (child)
                                 (lg-view! lg-syntax-node-kind child))
                              children)
                      '(alpha beta))))))

(ert-deftest lg-syntax-node-children-list-slice-inserts-effectfully ()
  (with-temp-buffer
    (insert "alpha beta")
    (setq-local major-mode 'lg-syntax-test-mode)
    (let* ((buffer (current-buffer))
           (root-optic (lg-compose (lg-syntax-node-ref-at-point 2)
                                   lg-syntax-node-ref-parent))
           (children-optic (lg-compose root-optic
                                       lg-syntax-node-children-list))
           (children (lg-view! children-optic buffer))
           (beta (cadr children)))
      (lg-set! (lg-compose children-optic (lg-slice 1 1))
               (list beta)
               buffer)
      (should (equal (buffer-string) "alpha beta beta"))
      (should (equal (mapcar (lambda (child)
                                (lg-view! lg-syntax-node-kind child))
                              (lg-view! children-optic buffer))
                      '(alpha beta beta))))))

(ert-deftest lg-syntax-node-ref-missing-adapter-returns-nothing ()
  (with-temp-buffer
    (insert "alpha beta")
    (let ((buffer (current-buffer)))
      (should (equal (lg-preview! (lg-syntax-node-ref-at-point 2) buffer)
                      lg-nothing)))))

(provide 'looking-glass-syntax-test)

;;; looking-glass-syntax-test.el ends here
