;;; looking-glass-syntax-treesit.el --- Tree-sitter adapter for looking-glass syntax optics -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (cl-lib "0.6") (looking-glass-syntax "0.1.0"))
;; Keywords: lisp, convenience, tools

;;; Commentary:

;; Tree-sitter-backed implementation of the generic syntax optics interface.

;;; Code:

(require 'cl-lib)
(require 'looking-glass-syntax)

(declare-function treesit-available-p "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-parser-language "treesit")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-child-count "treesit")
(declare-function treesit-node-child "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-text "treesit")

(defun lg-syntax-treesit--node-text (node)
  "Return NODE text content without properties."
  (condition-case nil
      (treesit-node-text node t)
    (wrong-number-of-arguments
     (treesit-node-text node))))

(defun lg-syntax-treesit--child-separator (children)
  "Infer separator text used between CHILDREN in current buffer."
  (let ((separator " "))
    (when (> (length children) 1)
      (let ((first (nth 0 children))
            (second (nth 1 children)))
        (setq separator
              (buffer-substring-no-properties
               (treesit-node-end first)
               (treesit-node-start second)))))
    separator))

(defun lg-syntax-treesit--empty-children-range (node)
  "Return replacement range for NODE when it currently has no children."
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (text (lg-syntax-treesit--node-text node))
         (open (and (> (length text) 0) (aref text 0)))
         (close (and (> (length text) 1) (aref text (1- (length text))))))
    (if (or (and (eq open ?\() (eq close ?\)))
            (and (eq open ?\[) (eq close ?\]))
            (and (eq open ?\{) (eq close ?\})))
        (cons (1+ start) (1- end))
      (cons end end))))

(defun lg-syntax-treesit--refresh-node (adapter buffer original)
  "Return refreshed equivalent of ORIGINAL node in BUFFER.
Matching prefers same start position and node kind."
  (let* ((target-start (treesit-node-start original))
         (target-kind (treesit-node-type original))
         (candidate (lg-syntax-node-at-point adapter buffer target-start)))
    (while (and candidate
                (not (and (= (treesit-node-start candidate) target-start)
                          (equal (treesit-node-type candidate) target-kind))))
      (setq candidate (treesit-node-parent candidate)))
    (or candidate original)))

(cl-defstruct lg-syntax-treesit-adapter
  language)

(defun lg-syntax-treesit--available-p ()
  "Return non-nil when tree-sitter APIs are available and enabled."
  (and (fboundp 'treesit-available-p)
       (condition-case nil
           (treesit-available-p)
         (error nil))))

(defun lg-syntax-treesit--buffer-language ()
  "Return primary parser language for current buffer, or nil."
  (when (and (fboundp 'treesit-parser-list)
             (fboundp 'treesit-parser-language))
    (let ((parsers (condition-case nil
                       (treesit-parser-list)
                     (error nil))))
      (when parsers
        (condition-case nil
            (treesit-parser-language (car parsers))
          (error nil))))))

(cl-defmethod lg-syntax-adapter-for-mode ((_mode symbol))
  (when (lg-syntax-treesit--available-p)
    (let ((parsers (and (fboundp 'treesit-parser-list)
                        (condition-case nil
                            (treesit-parser-list)
                          (error nil)))))
      (when parsers
        (make-lg-syntax-treesit-adapter :language (lg-syntax-treesit--buffer-language))))))

(defun lg-syntax-treesit--node-at (point language)
  "Return tree-sitter node at POINT using LANGUAGE when provided."
  (if language
      (condition-case nil
          (treesit-node-at point language)
        (wrong-number-of-arguments (treesit-node-at point))
        (error nil))
    (condition-case nil
        (treesit-node-at point)
      (error nil))))

(cl-defmethod lg-syntax-node-at-point ((adapter lg-syntax-treesit-adapter)
                                       (buffer buffer)
                                       point)
  (when (and (lg-syntax-treesit--available-p)
             (fboundp 'treesit-node-at))
    (with-current-buffer buffer
      (lg-syntax-treesit--node-at point (lg-syntax-treesit-adapter-language adapter)))))

(cl-defmethod lg-syntax-node-parent ((_adapter lg-syntax-treesit-adapter)
                                     (_buffer buffer)
                                     node)
  (when (fboundp 'treesit-node-parent)
    (condition-case nil
        (treesit-node-parent node)
      (error nil))))

(cl-defmethod lg-syntax-node-children ((_adapter lg-syntax-treesit-adapter)
                                       (_buffer buffer)
                                       node)
  (if (and (fboundp 'treesit-node-child-count)
           (fboundp 'treesit-node-child))
      (let ((count (condition-case nil
                       (treesit-node-child-count node)
                     (error 0)))
            children)
        (dotimes (index count (nreverse children))
          (let ((child (condition-case nil
                           (treesit-node-child node index)
                         (wrong-number-of-arguments
                          (treesit-node-child node index nil))
                         (error nil))))
            (when child
              (push child children)))))
    nil))

(cl-defmethod lg-syntax-node-bounds-of ((_adapter lg-syntax-treesit-adapter)
                                        (_buffer buffer)
                                        node)
  (cons (treesit-node-start node)
        (treesit-node-end node)))

(cl-defmethod lg-syntax-node-kind-of ((_adapter lg-syntax-treesit-adapter)
                                      (_buffer buffer)
                                      node)
  (treesit-node-type node))

(cl-defmethod lg-syntax-node-text-of ((_adapter lg-syntax-treesit-adapter)
                                      (_buffer buffer)
                                      node)
  (lg-syntax-treesit--node-text node))

(cl-defmethod lg-syntax-node-set-children ((adapter lg-syntax-treesit-adapter)
                                           (buffer buffer)
                                           (node t)
                                           (children list))
  (let* ((existing-children (lg-syntax-node-children adapter buffer node))
         (range (if existing-children
                    (cons (treesit-node-start (car existing-children))
                          (treesit-node-end (car (last existing-children))))
                  (lg-syntax-treesit--empty-children-range node))))
    (with-current-buffer buffer
      (save-excursion
        (let* ((replace-start (car range))
               (replace-end (cdr range))
               (separator (if existing-children
                              (lg-syntax-treesit--child-separator existing-children)
                            " "))
               (replacement
                (mapconcat #'lg-syntax-treesit--node-text children separator)))
          (goto-char replace-start)
          (delete-region replace-start replace-end)
          (insert replacement))))
    (lg-syntax-treesit--refresh-node adapter buffer node)))

(provide 'looking-glass-syntax-treesit)

;;; looking-glass-syntax-treesit.el ends here
