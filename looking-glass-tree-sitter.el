;;; looking-glass-tree-sitter.el --- Tree-sitter optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (looking-glass "0.4.0") (looking-glass-buffer "0.1.0"))
;; Keywords: lisp, data, treesit, tools
;; URL: https://github.com/fitzgibbon/looking-glass

;;; Commentary:

;; Tree-sitter-powered optics for querying and rewriting buffer AST slices.

;;; Code:

(require 'cl-lib)
(require 'looking-glass)
(require 'looking-glass-buffer)
(require 'treesit nil t)

(defun lg-ts--ensure-live-buffer (buffer)
  "Return BUFFER when it is live, else signal an error."
  (unless (buffer-live-p buffer)
    (error "Expected live buffer, got: %S" buffer))
  buffer)

(defun lg-ts--ensure-treesit ()
  "Signal if tree-sitter support is unavailable."
  (unless (fboundp 'treesit-query-capture)
    (error "Tree-sitter support is not available in this Emacs")))

(defun lg-ts--root-node (buffer &optional language)
  "Return tree-sitter root node for BUFFER and optional LANGUAGE."
  (lg-ts--ensure-treesit)
  (lg-ts--ensure-live-buffer buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (if language
          (treesit-buffer-root-node language)
        (treesit-buffer-root-node)))))

(defun lg-ts--captures-for-query (buffer query &optional language capture)
  "Return normalized captures for QUERY in BUFFER.

Optional LANGUAGE selects the parser. Optional CAPTURE filters capture name.
Each element is a plist containing :capture, :node, :start, :end, :text,
and :index where index is (CAPTURE . ORDINAL)."
  (let ((root (lg-ts--root-node buffer language))
        (out nil)
        (ordinal 0))
    (dolist (entry (treesit-query-capture root query nil nil nil))
      (let ((cap (car entry))
            (node (cdr entry)))
        (when (or (null capture) (eq capture cap))
          (let ((start (treesit-node-start node))
                (end (treesit-node-end node)))
            (push (list :capture cap
                        :node node
                        :start start
                        :end end
                        :text (buffer-substring-no-properties start end)
                        :index (cons cap ordinal))
                  out)
            (setq ordinal (1+ ordinal))))))
    (setq out (nreverse out))
    (setq out (sort out (lambda (a b)
                          (let ((sa (plist-get a :start))
                                (sb (plist-get b :start))
                                (ea (plist-get a :end))
                                (eb (plist-get b :end)))
                            (or (< sa sb)
                                (and (= sa sb) (< ea eb)))))))
    (let ((prev-end nil))
      (dolist (item out)
        (let ((start (plist-get item :start))
              (end (plist-get item :end)))
          (when (and prev-end (< start prev-end))
            (error "Query produced overlapping captures at %d..%d" start end))
          (setq prev-end end))))
    out))

(defun lg-ts--replace-text (buffer text)
  "Replace full BUFFER text with TEXT and return BUFFER."
  (lg-ts--ensure-live-buffer buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (lg-set (lg-buffer-string) text buffer))))

(defun lg-ts--query-traverse-builder (query language capture indexed)
  "Build traversal implementation for QUERY over buffer text.

When INDEXED is non-nil, STEP receives (INDEX . TEXT) where INDEX is
(CAPTURE . ORDINAL)."
  (lambda (step source app)
    (lg-ts--ensure-live-buffer source)
    (let* ((caps (lg-ts--captures-for-query source query language capture))
           (whole (with-current-buffer source
                    (save-restriction
                      (widen)
                      (buffer-substring-no-properties (point-min) (point-max)))))
           (base (with-current-buffer source
                   (save-restriction
                     (widen)
                     (point-min))))
           (pure (lg-applicative-pure app))
           (map2 (lg-applicative-map2 app))
           (cursor 0)
           (acc nil))
      (setq acc (funcall pure ""))
      (dolist (item caps)
        (let* ((start (- (plist-get item :start) base))
               (end (- (plist-get item :end) base))
               (focus (plist-get item :text))
               (index (plist-get item :index)))
          (setq acc (funcall map2 #'concat acc
                             (funcall pure (substring whole cursor start))))
          (setq acc
                (funcall map2
                         #'concat
                         acc
                         (if indexed
                             (lg--app-map app #'cdr (funcall step (cons index focus)))
                           (funcall step focus))))
          (setq cursor end)))
      (setq acc (funcall map2 #'concat acc
                         (funcall pure (substring whole cursor))))
      (lg--app-map app
                   (lambda (new-text)
                     (lg-ts--replace-text source new-text))
                   acc))))

(defun lg-ts-root-node (&optional language)
  "Getter focusing the tree-sitter root node of a live buffer.

Optional LANGUAGE selects a parser when multiple parsers are present."
  (lg-getter
   (lambda (source)
     (lg-ts--root-node source language))))

(defun lg-ts-query-nodes (query &optional language capture)
  "Traversal over nodes captured by tree-sitter QUERY in a live buffer.

Optional LANGUAGE selects parser. Optional CAPTURE filters capture names."
  (lg-wander
   (lambda (step source app)
     (let ((map2 (lg-applicative-map2 app))
           (acc (funcall (lg-applicative-pure app) nil)))
       (dolist (item (lg-ts--captures-for-query source query language capture))
         (setq acc
               (funcall map2
                        (lambda (xs y)
                          (append xs (list y)))
                        acc
                        (funcall step (plist-get item :node)))))
       (lg--app-map app (lambda (_updated) source) acc)))))

(defun lg-its-query-nodes (query &optional language capture)
  "Indexed traversal over nodes captured by tree-sitter QUERY in a live buffer.

Indices are (CAPTURE . ORDINAL)."
  (lg-iwander
   (lambda (step source app)
     (let ((map2 (lg-applicative-map2 app))
           (acc (funcall (lg-applicative-pure app) nil)))
       (dolist (item (lg-ts--captures-for-query source query language capture))
         (setq acc
               (funcall map2
                        (lambda (xs y)
                          (append xs (list y)))
                        acc
                        (funcall step (cons (plist-get item :index)
                                            (plist-get item :node))))))
       (lg--app-map app (lambda (_updated) source) acc)))))

(defun lg-ts-query (query &optional language capture)
  "Traversal over captured node text from tree-sitter QUERY in a live buffer.

Optional LANGUAGE selects parser. Optional CAPTURE filters capture names.
Each focus is the exact source text for one capture.

Writes rebuild the full buffer text and preserve non-captured spans."
  (lg-wander (lg-ts--query-traverse-builder query language capture nil)))

(defun lg-its-query (query &optional language capture)
  "Indexed traversal over captured node text from tree-sitter QUERY.

Indices are (CAPTURE . ORDINAL)."
  (lg-iwander (lg-ts--query-traverse-builder query language capture t)))

(provide 'looking-glass-tree-sitter)

;;; looking-glass-tree-sitter.el ends here
