;;; looking-glass-syntax.el --- Syntax navigation optics for looking-glass -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6") (looking-glass "0.1.0"))
;; Keywords: lisp, convenience, tools

;;; Commentary:

;; Generic syntax-tree interfaces and optics that can be implemented per mode.

;;; Code:

(require 'cl-lib)
(require 'looking-glass)

(cl-defstruct lg-syntax-ref
  buffer
  adapter
  node)

(cl-defstruct lg-syntax-op-node-text-set
  ref
  text)

(defun lg-syntax--live-buffer (value)
  "Return VALUE when it is a live buffer, or signal otherwise."
  (if (buffer-live-p value)
      value
    (error "Expected live buffer")))

(defun lg-syntax--ensure-ref (value)
  "Return VALUE when it is an `lg-syntax-ref', or signal otherwise."
  (if (lg-syntax-ref-p value)
      value
    (error "Expected syntax ref")))

(defun lg-syntax--traverse-list (applicative afb source)
  "Traverse SOURCE list with AFB using APPLICATIVE."
  (let ((pure (lg-applicative-pure applicative))
        (ap (lg-applicative-ap applicative))
        (fmap (lg-applicative-fmap applicative)))
    (funcall fmap
             #'nreverse
             (cl-reduce
              (lambda (acc focus)
                (funcall ap
                         (funcall fmap
                                  (lambda (partial)
                                    (lambda (item) (cons item partial)))
                                  acc)
                         (funcall afb focus)))
              source
              :initial-value (funcall pure nil)))))

(cl-defgeneric lg-syntax-adapter-for-mode (mode)
  "Return syntax adapter for major MODE, or nil when unsupported.")

(cl-defmethod lg-syntax-adapter-for-mode ((_mode t))
  nil)

(defun lg-syntax-adapter-for-buffer (buffer)
  "Return syntax adapter for BUFFER's major mode, or nil when unsupported."
  (with-current-buffer (lg-syntax--live-buffer buffer)
    (lg-syntax-adapter-for-mode major-mode)))

(cl-defgeneric lg-syntax-node-at-point (adapter buffer point)
  "Return syntax node in BUFFER at POINT for ADAPTER, or nil.")

(cl-defgeneric lg-syntax-node-parent (adapter buffer node)
  "Return parent syntax node for NODE in BUFFER for ADAPTER, or nil.")

(cl-defgeneric lg-syntax-node-children (adapter buffer node)
  "Return child syntax nodes for NODE in BUFFER for ADAPTER.")

(cl-defgeneric lg-syntax-node-bounds-of (adapter buffer node)
  "Return NODE bounds in BUFFER as (START . END) for ADAPTER.")

(cl-defgeneric lg-syntax-node-kind-of (adapter buffer node)
  "Return NODE kind identifier for ADAPTER.")

(cl-defgeneric lg-syntax-node-text-of (adapter buffer node)
  "Return textual content of NODE in BUFFER for ADAPTER.")

(cl-defgeneric lg-syntax-node-replace-text (adapter buffer node text)
  "Replace NODE text in BUFFER with TEXT for ADAPTER.
Return replacement node when available; otherwise nil.")

(cl-defmethod lg-syntax-node-parent ((_adapter t) (_buffer buffer) (_node t))
  nil)

(cl-defmethod lg-syntax-node-children ((_adapter t) (_buffer buffer) (_node t))
  nil)

(cl-defmethod lg-syntax-node-text-of ((adapter t) (buffer buffer) (node t))
  (let* ((bounds (lg-syntax-node-bounds-of adapter buffer node))
         (start (car bounds))
         (end (cdr bounds)))
    (with-current-buffer (lg-syntax--live-buffer buffer)
      (buffer-substring-no-properties start end))))

(cl-defmethod lg-syntax-node-replace-text ((adapter t) (buffer buffer) (node t) text)
  (let* ((bounds (lg-syntax-node-bounds-of adapter buffer node))
         (start (car bounds))
         (end (cdr bounds)))
    (unless (stringp text)
      (error "Expected replacement text string"))
    (with-current-buffer (lg-syntax--live-buffer buffer)
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert text)))
    (lg-syntax-node-at-point adapter buffer start)))

(defun lg-syntax--resolve-adapter (buffer adapter)
  "Resolve syntax adapter for BUFFER using ADAPTER override when provided."
  (or adapter
      (lg-syntax-adapter-for-buffer buffer)))

(defun lg-syntax-node-ref-at-point (&optional point adapter)
  "Affine traversal focusing syntax node ref at POINT in a buffer.
When POINT is nil, current point is used in the source buffer.
When ADAPTER is nil, adapter is resolved from the buffer major mode."
  (lg-affine
   (lambda (buffer)
     (let* ((live (lg-syntax--live-buffer buffer))
            (resolved-adapter (lg-syntax--resolve-adapter live adapter))
            (resolved-point (or point (with-current-buffer live (point)))))
       (if resolved-adapter
           (let ((node (lg-syntax-node-at-point resolved-adapter live resolved-point)))
             (if node
                 (lg-just (make-lg-syntax-ref :buffer live
                                              :adapter resolved-adapter
                                              :node node))
               lg-nothing))
         lg-nothing)))
   (lambda (buffer replacement)
     (let ((ref (lg-syntax--ensure-ref replacement)))
       (unless (eq (lg-syntax-ref-buffer ref) (lg-syntax--live-buffer buffer))
         (error "Replacement ref must point at source buffer"))
       buffer))))

(defconst lg-syntax-node-ref-parent
  (lg-affine
   (lambda (ref)
     (setq ref (lg-syntax--ensure-ref ref))
     (let* ((buffer (lg-syntax-ref-buffer ref))
            (adapter (lg-syntax-ref-adapter ref))
            (node (lg-syntax-ref-node ref))
            (parent (lg-syntax-node-parent adapter buffer node)))
       (if parent
           (lg-just (make-lg-syntax-ref :buffer buffer :adapter adapter :node parent))
         lg-nothing)))
   (lambda (_ref replacement)
     (lg-syntax--ensure-ref replacement)))
  "Affine traversal focusing parent syntax node from a syntax ref.")

(defconst lg-syntax-node-ref-children
  (lg-traversal
   (lambda (afb source applicative)
     (setq source (lg-syntax--ensure-ref source))
     (let* ((buffer (lg-syntax-ref-buffer source))
            (adapter (lg-syntax-ref-adapter source))
            (node (lg-syntax-ref-node source))
            (fmap (lg-applicative-fmap applicative))
            (children (lg-syntax-node-children adapter buffer node))
            (child-refs
             (mapcar (lambda (child)
                       (make-lg-syntax-ref :buffer buffer :adapter adapter :node child))
                     children)))
       (funcall fmap
                (lambda (_replacements) source)
                (lg-syntax--traverse-list applicative afb child-refs)))))
  "Traversal focusing child syntax refs from a syntax ref.
Child traversal is read-oriented: updates keep the original source ref.")

(defconst lg-syntax-node-kind
  (lg-getter
   (lambda (ref)
     (setq ref (lg-syntax--ensure-ref ref))
     (lg-syntax-node-kind-of (lg-syntax-ref-adapter ref)
                             (lg-syntax-ref-buffer ref)
                             (lg-syntax-ref-node ref))))
  "Getter focusing syntax node kind from a syntax ref.")

(defconst lg-syntax-node-bounds
  (lg-getter
   (lambda (ref)
     (setq ref (lg-syntax--ensure-ref ref))
     (lg-syntax-node-bounds-of (lg-syntax-ref-adapter ref)
                               (lg-syntax-ref-buffer ref)
                               (lg-syntax-ref-node ref))))
  "Getter focusing syntax node bounds from a syntax ref.")

(cl-defmethod lg-effect-handle ((_buffer buffer) (operation lg-syntax-op-node-text-set))
  (let* ((ref (lg-syntax-op-node-text-set-ref operation))
         (text (lg-syntax-op-node-text-set-text operation))
         (buffer (lg-syntax-ref-buffer ref))
         (adapter (lg-syntax-ref-adapter ref))
         (node (lg-syntax-ref-node ref))
         (replacement (lg-syntax-node-replace-text adapter buffer node text)))
    (make-lg-syntax-ref :buffer buffer
                        :adapter adapter
                        :node (or replacement node))))

(defconst lg-syntax-node-text
  (lg-lens
   (lambda (ref)
     (setq ref (lg-syntax--ensure-ref ref))
     (lg-syntax-node-text-of (lg-syntax-ref-adapter ref)
                             (lg-syntax-ref-buffer ref)
                             (lg-syntax-ref-node ref)))
   (lambda (ref replacement)
     (lg-syntax-set-node-text replacement ref)))
  "Lens focusing syntax node text from a syntax ref.")

(defun lg-syntax-set-node-text (text ref)
  "Replace REF node text with TEXT through the effect system.
Return updated syntax ref."
  (setq ref (lg-syntax--ensure-ref ref))
  (lg-run-effect
   (lg-effect-perform
    (make-lg-syntax-op-node-text-set :ref ref :text text))
   (lg-syntax-ref-buffer ref)))

(defun lg-syntax-over-node-text (fn ref)
  "Transform REF node text by applying FN through the effect system.
Return updated syntax ref."
  (lg-syntax-set-node-text
   (funcall fn (lg-view lg-syntax-node-text ref))
   ref))

(provide 'looking-glass-syntax)

;;; looking-glass-syntax.el ends here
