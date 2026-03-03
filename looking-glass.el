;;; looking-glass.el --- Profunctor optics for Emacs Lisp -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6"))
;; Keywords: lisp, extensions

;;; Commentary:

;; looking-glass provides a profunctor-based optics core for Emacs Lisp.

;;; Code:

(require 'cl-lib)
(require 'json)

(define-error 'lg-no-focus "No focus found")
(define-error 'lg-expected-non-nil "Expected non-nil focus")

(cl-defstruct lg-monoid
  empty
  append)

(cl-defstruct lg-applicative
  pure
  ap
  fmap)

(cl-defstruct lg-identity
  value)

(cl-defstruct lg-const
  value)

(cl-defstruct lg-profunctor
  dimap
  first
  right
  wander)

(cl-defstruct lg-indexed-profunctor
  dimap
  first
  right
  wander
  reindex)

(cl-defstruct lg-optic
  apply
  review)

(cl-defstruct lg-indexed-optic
  apply)

(defun lg-left (value)
  "Construct a left Either VALUE."
  (cons 'left value))

(defun lg-right (value)
  "Construct a right Either VALUE."
  (cons 'right value))

(defun lg-left-p (either)
  "Return non-nil when EITHER is a left value."
  (eq (car either) 'left))

(defun lg-right-p (either)
  "Return non-nil when EITHER is a right value."
  (eq (car either) 'right))

(defun lg-either-p (value)
  "Return non-nil when VALUE is a tagged Either.
Accepted shapes are `(left . X)' and `(right . X)'."
  (and (consp value)
       (or (eq (car value) 'left)
           (eq (car value) 'right))))

(defun lg-either-value (either)
  "Return payload value from tagged EITHER.
Signals when EITHER is not a tagged either value."
  (if (lg-either-p either)
      (cdr either)
    (error "Expected tagged either value")))

(defconst lg-nothing 'lg-nothing
  "Tagged Maybe value representing an absent focus.")

(defconst lg-true 'lg-true
  "Tagged boolean value representing true.")

(defconst lg-false 'lg-false
  "Tagged boolean value representing false.")

(defun lg-just (value)
  "Construct a tagged Maybe value containing VALUE."
  (cons 'lg-just value))

(defun lg-just-p (maybe)
  "Return non-nil when MAYBE is a tagged present value."
  (and (consp maybe) (eq (car maybe) 'lg-just)))

(defun lg-nothing-p (maybe)
  "Return non-nil when MAYBE is tagged as absent."
  (eq maybe lg-nothing))

(defun lg-maybe-value (maybe)
  "Return payload for tagged MAYBE, or nil when absent."
  (if (lg-just-p maybe) (cdr maybe) nil))

(defun lg-bool-p (value)
  "Return non-nil when VALUE is a tagged lg boolean value."
  (or (eq value lg-true)
      (eq value lg-false)))

(defun lg-bool-value (value)
  "Convert tagged lg boolean VALUE to Elisp t/nil."
  (cond
   ((eq value lg-true) t)
   ((eq value lg-false) nil)
   (t (error "Expected lg boolean value"))))

(defun lg-bool (value)
  "Convert Elisp boolean VALUE (t/nil) to tagged lg boolean."
  (if (booleanp value)
      (if value lg-true lg-false)
    (error "Expected Elisp boolean (t or nil)")))

(defun lg--json-map-values-list (list-value fn)
  "Map FN over LIST-VALUE preserving proper list shape."
  (mapcar fn list-value))

(defun lg--json-map-values-vector (vector-value fn)
  "Map FN over VECTOR-VALUE preserving vector shape."
  (vconcat (mapcar fn (append vector-value nil))))

(defun lg--json-normalize-from-parser (value false-object)
  "Normalize parser VALUE into looking-glass JSON domain.
Map booleans to `lg-true'/`lg-false' and preserve nil for JSON null.
FALSE-OBJECT is the parser representation for false values."
  (cond
   ((eq value t) lg-true)
   ((equal value false-object) lg-false)
   ((hash-table-p value)
    (let ((copy (copy-hash-table value)))
      (maphash (lambda (key item)
                 (puthash key (lg--json-normalize-from-parser item false-object) copy))
               value)
      copy))
   ((and (listp value) (cl-every #'consp value))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (lg--json-normalize-from-parser (cdr entry) false-object)))
            value))
   ((and (listp value)
         (zerop (% (length value) 2))
         (cl-loop for rest on value by #'cddr
                  always (symbolp (car rest))))
    (let ((rest value)
          (result nil))
      (while rest
        (setq result
              (append result
                      (list (car rest)
                            (lg--json-normalize-from-parser (cadr rest) false-object))))
        (setq rest (cddr rest)))
      result))
   ((vectorp value)
    (lg--json-map-values-vector
     value
     (lambda (item)
       (lg--json-normalize-from-parser item false-object))))
   ((listp value)
    (lg--json-map-values-list
     value
     (lambda (item)
       (lg--json-normalize-from-parser item false-object))))
   (t value)))

(defun lg--json-normalize-for-serializer (value false-object)
  "Normalize VALUE for `json-serialize'.
Map `lg-true'/`lg-false' to serializer booleans and ensure arrays are vectors.
FALSE-OBJECT is the serializer representation for false values."
  (cond
   ((eq value lg-true) t)
   ((eq value lg-false) false-object)
   ((hash-table-p value)
    (let ((copy (copy-hash-table value)))
      (maphash (lambda (key item)
                 (puthash key (lg--json-normalize-for-serializer item false-object) copy))
               value)
      copy))
   ((and (listp value) (cl-every #'consp value))
    (mapcar (lambda (entry)
              (cons (car entry)
                    (lg--json-normalize-for-serializer (cdr entry) false-object)))
            value))
   ((and (listp value)
         (zerop (% (length value) 2))
         (cl-loop for rest on value by #'cddr
                  always (symbolp (car rest))))
    (let ((rest value)
          (result nil))
      (while rest
        (setq result
              (append result
                      (list (car rest)
                            (lg--json-normalize-for-serializer (cadr rest) false-object))))
        (setq rest (cddr rest)))
      result))
   ((vectorp value)
    (lg--json-map-values-vector
     value
     (lambda (item)
       (lg--json-normalize-for-serializer item false-object))))
   ((listp value)
    (vconcat
     (mapcar (lambda (item)
               (lg--json-normalize-for-serializer item false-object))
             value)))
   (t value)))

(defun lg-json-parse-with (object-type array-type &optional null-object false-object)
  "Return a prism between JSON text and Elisp values.
OBJECT-TYPE and ARRAY-TYPE are passed to `json-parse-string'.
NULL-OBJECT and FALSE-OBJECT are used for parse/render, defaulting to nil and `lg-false'.
Values are normalized so booleans become `lg-true'/`lg-false'."
  (let ((null-value (if (null null-object) nil null-object))
        (false-value (if (null false-object) lg-false false-object)))
    (lg-prism
     (lambda (json-text)
       (if (stringp json-text)
           (condition-case nil
               (lg-right
                (lg--json-normalize-from-parser
                 (json-parse-string json-text
                                    :object-type object-type
                                    :array-type array-type
                                    :null-object null-value
                                    :false-object false-value)
                 false-value))
             (error (lg-left json-text)))
         (lg-left json-text)))
     (lambda (value)
       (json-serialize
        (lg--json-normalize-for-serializer value false-value)
        :null-object null-value
        :false-object false-value)))))

(defun lg--identity-applicative ()
  "Return the identity applicative."
  (make-lg-applicative
   :pure (lambda (value) (make-lg-identity :value value))
   :ap (lambda (wrapped-fn wrapped-value)
         (make-lg-identity
          :value
          (funcall (lg-identity-value wrapped-fn)
                   (lg-identity-value wrapped-value))))
   :fmap (lambda (fn wrapped-value)
           (make-lg-identity :value (funcall fn (lg-identity-value wrapped-value))))))

(defun lg--const-applicative (monoid)
  "Return a const applicative for MONOID."
  (let ((mempty (lg-monoid-empty monoid))
        (mappend (lg-monoid-append monoid)))
    (make-lg-applicative
     :pure (lambda (_value) (make-lg-const :value mempty))
     :ap (lambda (wrapped-fn wrapped-value)
           (make-lg-const
            :value
            (funcall mappend
                     (lg-const-value wrapped-fn)
                     (lg-const-value wrapped-value))))
     :fmap (lambda (_fn wrapped-value) wrapped-value))))

(defun lg--star-profunctor (applicative)
  "Return Star profunctor implementation over APPLICATIVE."
  (let ((fmap (lg-applicative-fmap applicative)))
    (make-lg-profunctor
     :dimap (lambda (before after pab)
              (lambda (value)
                (funcall fmap
                         after
                         (funcall pab (funcall before value)))))
     :first (lambda (pab)
              (lambda (pair)
                (let ((left (car pair))
                      (right (cdr pair)))
                  (funcall fmap
                           (lambda (new-left) (cons new-left right))
                           (funcall pab left)))))
     :right (lambda (pab)
              (lambda (either)
                (if (lg-right-p either)
                    (funcall fmap
                             (lambda (new-right) (lg-right new-right))
                             (funcall pab (cdr either)))
                  (funcall (lg-applicative-pure applicative)
                           (lg-left (cdr either))))))
     :wander (lambda (wander-fn pab)
               (lambda (value)
                 (funcall wander-fn pab value applicative))))))

(defun lg--indexed-star-profunctor (applicative)
  "Return Star profunctor for indexed optics over APPLICATIVE.
The mapping function receives (INDEX FOCUS)."
  (let ((fmap (lg-applicative-fmap applicative)))
    (make-lg-indexed-profunctor
     :dimap (lambda (before after pib)
              (lambda (value)
                (funcall fmap
                         after
                         (funcall pib nil (funcall before value)))))
     :first (lambda (pib)
              (lambda (pair)
                (let ((left (car pair))
                      (right (cdr pair)))
                  (funcall fmap
                           (lambda (new-left) (cons new-left right))
                           (funcall pib nil left)))))
     :right (lambda (pib)
              (lambda (either)
                (if (lg-right-p either)
                    (funcall fmap
                             (lambda (new-right) (lg-right new-right))
                             (funcall pib nil (cdr either)))
                  (funcall (lg-applicative-pure applicative)
                           (lg-left (cdr either))))))
     :wander (lambda (wander-fn pib)
               (lambda (value)
                 (funcall wander-fn pib value applicative)))
     :reindex (lambda (index-fn pib)
                (lambda (index focus)
                  (funcall pib (funcall index-fn index) focus))))))

(defun lg--run-optic (optic profunctor pab)
  "Run OPTIC against PROFUNCTOR using PAB."
  (funcall (lg-optic-apply optic) profunctor pab))

(defun lg--run-indexed-optic (optic indexed-profunctor pib)
  "Run indexed OPTIC against INDEXED-PROFUNCTOR using PIB."
  (funcall (lg-indexed-optic-apply optic) indexed-profunctor pib))

(defconst lg-id
  (make-lg-optic :apply (lambda (_p pab) pab)
                 :review #'identity)
  "Identity optic.")

(defconst lg-indexed-id
  (make-lg-indexed-optic :apply (lambda (_p pib) pib))
  "Identity indexed optic.")

(defun lg-compose2 (outer inner)
  "Compose OUTER with INNER.
The resulting optic focuses through INNER first, then OUTER."
  (make-lg-optic
   :apply (lambda (p pab)
            (lg--run-optic inner p (lg--run-optic outer p pab)))
   :review (let ((outer-review (lg-optic-review outer))
                 (inner-review (lg-optic-review inner)))
             (when (and outer-review inner-review)
               (lambda (value)
                 (funcall outer-review (funcall inner-review value)))))))

(defun lg-compose (&rest optics)
  "Compose OPTICS from right to left."
  (if optics
      (cl-reduce #'lg-compose2 optics :from-end t)
    lg-id))

(defun lg-compose-indexed2 (outer inner)
  "Compose indexed OUTER with indexed INNER."
  (make-lg-indexed-optic
   :apply (lambda (p pib)
            (lg--run-indexed-optic inner p (lg--run-indexed-optic outer p pib)))))

(defun lg-compose-indexed (&rest optics)
  "Compose indexed OPTICS from right to left."
  (if optics
      (cl-reduce #'lg-compose-indexed2 optics :from-end t)
    lg-indexed-id))

(defun lg-iso (forward backward)
  "Build an isomorphism from FORWARD and BACKWARD."
  (make-lg-optic
   :apply (lambda (p pab)
            (funcall (lg-profunctor-dimap p) forward backward pab))
   :review backward))

(defconst lg-unbool
  (lg-iso #'lg-bool-value #'lg-bool)
  "Iso between tagged lg booleans and Elisp t/nil booleans.")

(defun lg-lens (getter setter)
  "Build a lens using GETTER and SETTER.
SETTER is called as (SETTER source new-focus)."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((first (lg-profunctor-first p)))
              (unless first
                (error "Lens requires profunctor first"))
              (funcall (lg-profunctor-dimap p)
                       (lambda (source)
                         (cons (funcall getter source) source))
                       (lambda (focused-and-source)
                         (funcall setter
                                  (cdr focused-and-source)
                                  (car focused-and-source)))
                       (funcall first pab))))))

(defun lg-prism (match build)
  "Build a prism from MATCH and BUILD.
MATCH must return (lg-left t) or (lg-right a).
BUILD maps b to t."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((right (lg-profunctor-right p)))
              (unless right
                (error "Prism requires profunctor right"))
              (funcall (lg-profunctor-dimap p)
                       match
                       (lambda (either)
                         (if (lg-right-p either)
                             (funcall build (cdr either))
                           (cdr either)))
                       (funcall right pab))))
   :review build))

(defconst lg-list->vector
  (lg-iso
   (lambda (value)
     (if (listp value)
         (vconcat value)
       (error "Expected list source for lg-list->vector")))
   (lambda (value)
     (if (vectorp value)
         (append value nil)
       (error "Expected vector focus for lg-list->vector"))))
  "Iso between list and vector.
Forward maps list -> vector, backward maps vector -> list.")

(defun lg--parse-number-string (value)
  "Parse VALUE into a number with full-string consumption.
Return the number on success, or nil on failure."
  (when (stringp value)
    (let ((parsed (ignore-errors (read-from-string value))))
      (when parsed
        (let ((candidate (car parsed))
              (position (cdr parsed)))
          (when (and (numberp candidate)
                     (string-match-p "\\`[[:space:]]*\\'" (substring value position)))
            candidate))))))

(defconst lg-number-string
  (lg-prism
   (lambda (value)
     (let ((parsed (lg--parse-number-string value)))
       (if parsed
           (lg-right parsed)
         (lg-left value))))
   (lambda (value)
     (if (numberp value)
         (number-to-string value)
       (error "Expected number focus for lg-number-string"))))
  "Prism between string and number.
Match parses string -> number; review renders number -> string.")

(defconst lg-char-string
  (lg-prism
   (lambda (value)
     (if (and (stringp value)
              (= (length value) 1)
              (characterp (aref value 0)))
         (lg-right (aref value 0))
       (lg-left value)))
   (lambda (value)
     (if (characterp value)
         (char-to-string value)
       (error "Expected character focus for lg-char-string"))))
  "Prism between string of length 1 and character code.")

(defconst lg-symbol-string
  (lg-prism
   (lambda (value)
     (if (stringp value)
         (let ((symbol (intern-soft value)))
           (if symbol
               (lg-right symbol)
             (lg-left value)))
       (lg-left value)))
   (lambda (value)
     (if (symbolp value)
         (symbol-name value)
       (error "Expected symbol focus for lg-symbol-string"))))
  "Prism between interned symbol names and strings.
Match uses `intern-soft' and fails when string is not interned.")

(defun lg--plist-even-p (plist)
  "Return non-nil when PLIST has even length."
  (zerop (% (length plist) 2)))

(defun lg--alist->plist-maybe (alist)
  "Return plist converted from ALIST, or nil when invalid.
Accepted entries are cons pairs with symbol keys and unique keys."
  (when (listp alist)
    (let ((seen (make-hash-table :test 'eq))
          (result nil)
          (ok t))
      (dolist (entry alist)
        (if (and (consp entry)
                 (symbolp (car entry))
                 (not (gethash (car entry) seen)))
            (progn
              (puthash (car entry) t seen)
              (setq result (append result (list (car entry) (cdr entry)))))
          (setq ok nil)))
      (and ok result))))

(defun lg--plist->alist-checked (plist)
  "Return alist converted from PLIST or signal on invalid data.
Accepted keys are symbols and keys must be unique."
  (unless (and (listp plist) (lg--plist-even-p plist))
    (error "Expected even-length plist"))
  (let ((seen (make-hash-table :test 'eq))
        (rest plist)
        (result nil))
    (while rest
      (let ((key (car rest))
            (value (cadr rest)))
        (unless (symbolp key)
          (error "Expected symbol key in plist"))
        (when (gethash key seen)
          (error "Duplicate key in plist: %S" key))
        (puthash key t seen)
        (setq result (append result (list (cons key value)))))
      (setq rest (cddr rest)))
    result))

(defconst lg-alist-plist
  (lg-prism
   (lambda (value)
     (let ((plist (lg--alist->plist-maybe value)))
       (if (or plist (null value))
           (lg-right (or plist nil))
         (lg-left value))))
   #'lg--plist->alist-checked)
  "Prism between constrained alist and constrained plist.
Accepted keys are unique symbols.")

(defun lg-review (optic value)
  "Construct a target value from VALUE using OPTIC review.
Signals when OPTIC does not support review."
  (let ((reviewer (lg-optic-review optic)))
    (if reviewer
        (funcall reviewer value)
      (error "Optic does not support review"))))

(defconst lg-json-parse
  (lg-json-parse-with 'hash-table 'array nil lg-false)
  "Default prism between JSON text and Elisp values.
Uses hash tables for objects, vectors for arrays, nil for null,
and `lg-true'/`lg-false' tagged booleans.")

(defun lg-traversal (wander-fn)
  "Build a traversal from WANDER-FN.
WANDER-FN is called as (WANDER-FN afb source applicative)."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((wander (lg-profunctor-wander p)))
              (unless wander
                (error "Traversal requires profunctor wander"))
              (funcall wander wander-fn pab)))))

(defun lg-indexed (wander-fn)
  "Build an indexed traversal from WANDER-FN.
WANDER-FN is called as (WANDER-FN iafb source applicative), where
IAFB is called as (IAFB index focus)."
  (make-lg-indexed-optic
   :apply (lambda (p pib)
            (let ((wander (lg-indexed-profunctor-wander p)))
              (unless wander
                (error "Indexed traversal requires indexed profunctor wander"))
              (funcall wander wander-fn pib)))))

(defun lg-ireindexed (index-fn optic)
  "Transform OPTIC indices with INDEX-FN."
  (make-lg-indexed-optic
   :apply (lambda (p pib)
            (let ((reindex (lg-indexed-profunctor-reindex p)))
              (unless reindex
                (error "Indexed optic requires indexed profunctor reindex"))
              (lg--run-indexed-optic optic p (funcall reindex index-fn pib))))))

(defun lg-getter (getter-fn)
  "Build a read-only getter optic from GETTER-FN.
Setter-like operations leave the source unchanged."
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                (lambda (_new-focus) source)
                (funcall afb (funcall getter-fn source)))))))

(defalias 'lg-to #'lg-getter)

(defun lg-filtered (predicate)
  "Traversal that focuses only values satisfying PREDICATE."
  (lg-traversal
   (lambda (afb source applicative)
     (if (funcall predicate source)
         (funcall afb source)
       (funcall (lg-applicative-pure applicative) source)))))

(defun lg-ifiltered (predicate)
  "Indexed traversal that focuses only values satisfying PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (lg-indexed
   (lambda (iafb source applicative)
     (if (funcall predicate nil source)
         (funcall iafb nil source)
       (funcall (lg-applicative-pure applicative) source)))))

(defun lg-indices (predicate)
  "Indexed traversal that focuses when index matches PREDICATE."
  (lg-ifiltered (lambda (index _focus) (funcall predicate index))))

(defun lg-indexed-list-filtered (predicate)
  "Indexed list traversal focused by PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (lg-indexed
   (lambda (iafb source applicative)
     (lg--traverse-list-indexed
      applicative
      (lambda (index focus)
        (if (funcall predicate index focus)
            (funcall iafb index focus)
          (funcall (lg-applicative-pure applicative) focus)))
      source))))

(defun lg-indexed-list-indices (predicate)
  "Indexed list traversal focused by index PREDICATE."
  (lg-indexed-list-filtered
   (lambda (index _focus)
     (funcall predicate index))))

(defun lg-required (optic)
  "Require non-nil focus for OPTIC by composing with `lg-non-nil'."
  (lg-compose lg-non-nil optic))

(defun lg--traverse-list (applicative afb source)
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

(defun lg--traverse-list-indexed (applicative iafb source)
  "Traverse SOURCE list with IAFB using APPLICATIVE.
IAFB is called as (IAFB index focus)."
  (let ((pure (lg-applicative-pure applicative))
        (ap (lg-applicative-ap applicative))
        (fmap (lg-applicative-fmap applicative))
        (index 0))
    (funcall fmap
             #'nreverse
             (cl-reduce
              (lambda (acc focus)
                (let ((current index))
                  (setq index (1+ index))
                  (funcall ap
                           (funcall fmap
                                    (lambda (partial)
                                      (lambda (item) (cons item partial)))
                                    acc)
                           (funcall iafb current focus))))
              source
              :initial-value (funcall pure nil)))))

(defun lg--plist-present-and-value (plist key testfn)
  "Return tagged maybe for KEY in PLIST using TESTFN."
  (let ((rest plist)
        found
        value)
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (when (and (not found) (funcall testfn candidate key))
          (setq found t
                value candidate-value))
        (setq rest (cddr rest))))
    (if found
        (lg-just value)
      lg-nothing)))

(defun lg--plist-update-first (plist key value testfn)
  "Return PLIST with first KEY set to VALUE using TESTFN."
  (let ((rest plist)
        (result nil)
        (updated nil))
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (if (and (not updated) (funcall testfn candidate key))
            (progn
              (setq updated t)
              (setq result (append result (list candidate value))))
          (setq result (append result (list candidate candidate-value)))))
      (setq rest (cddr rest)))
    result))

(defun lg--alist-present-and-value (alist key testfn)
  "Return tagged maybe for KEY in ALIST using TESTFN."
  (let ((cell (cl-find-if (lambda (entry)
                            (funcall testfn (car entry) key))
                          alist)))
    (if cell (lg-just (cdr cell)) lg-nothing)))

(defun lg--alist-update-first (alist key value testfn)
  "Return ALIST with first KEY set to VALUE using TESTFN."
  (let ((updated nil))
    (mapcar (lambda (entry)
              (if (and (not updated) (funcall testfn (car entry) key))
                  (progn
                    (setq updated t)
                    (cons (car entry) value))
                entry))
            alist)))

(defun lg--plist-remove-first (plist key testfn)
  "Return PLIST with first KEY removed using TESTFN."
  (let ((rest plist)
        (result nil)
        (removed nil))
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (if (and (not removed) (funcall testfn candidate key))
            (setq removed t)
          (setq result (append result (list candidate candidate-value)))))
      (setq rest (cddr rest)))
    result))

(defun lg--plist-set-first-or-add (plist key value testfn)
  "Return PLIST with first KEY set to VALUE, adding when missing."
  (if (lg-just-p (lg--plist-present-and-value plist key testfn))
      (lg--plist-update-first plist key value testfn)
    (append plist (list key value))))

(defun lg--alist-remove-first (alist key testfn)
  "Return ALIST with first KEY removed using TESTFN."
  (let ((result nil)
        (removed nil))
    (dolist (entry alist (nreverse result))
      (if (and (not removed) (funcall testfn (car entry) key))
          (setq removed t)
        (push entry result)))))

(defun lg--alist-set-first-or-add (alist key value testfn)
  "Return ALIST with first KEY set to VALUE, adding when missing."
  (if (lg-just-p (lg--alist-present-and-value alist key testfn))
      (lg--alist-update-first alist key value testfn)
    (append alist (list (cons key value)))))

(defun lg--alist-p (value)
  "Return non-nil when VALUE is an alist."
  (and (listp value)
       (cl-every #'consp value)))

(defun lg--keyed-kind (source)
  "Classify SOURCE as one supported keyed container kind."
  (cond
   ((hash-table-p source) 'hash-table)
   ((lg--alist-p source) 'alist)
   ((and (listp source) (zerop (% (length source) 2))) 'plist)
   (t nil)))

(defun lg--keyed-present-and-value (source key &optional testfn)
  "Return tagged maybe for KEY in SOURCE.
SOURCE can be plist, alist, or hash table."
  (pcase (lg--keyed-kind source)
    ('plist (lg--plist-present-and-value source key (or testfn #'eq)))
    ('alist (lg--alist-present-and-value source key (or testfn #'equal)))
    ('hash-table
     (let* ((missing (make-symbol "lg-absent"))
            (value (gethash key source missing)))
        (if (eq value missing)
            lg-nothing
          (lg-just value))))
    (_ (error "Unsupported keyed source type: %S" source))))

(defun lg--keyed-set-existing (source key value &optional testfn)
  "Set existing KEY to VALUE in SOURCE, preserving missing keys."
  (pcase (lg--keyed-kind source)
    ('plist
     (if (lg-just-p (lg--plist-present-and-value source key (or testfn #'eq)))
         (lg--plist-update-first source key value (or testfn #'eq))
       source))
    ('alist
     (if (lg-just-p (lg--alist-present-and-value source key (or testfn #'equal)))
         (lg--alist-update-first source key value (or testfn #'equal))
       source))
    ('hash-table
     (let* ((missing (make-symbol "lg-absent"))
            (current (gethash key source missing)))
       (if (eq current missing)
           source
         (let ((copy (copy-hash-table source)))
           (puthash key value copy)
           copy))))
    (_ (error "Unsupported keyed source type: %S" source))))

(defun lg--keyed-set-maybe (source key maybe &optional testfn)
  "Set presence state for KEY in SOURCE.
When MAYBE is `lg-nothing', remove KEY if present.
When MAYBE is `(lg-just . VALUE)', insert or update KEY with VALUE."
  (unless (or (lg-nothing-p maybe) (lg-just-p maybe))
    (error "Expected tagged maybe value"))
  (pcase (lg--keyed-kind source)
    ('plist
     (if (lg-just-p maybe)
         (lg--plist-set-first-or-add source key (cdr maybe) (or testfn #'eq))
       (lg--plist-remove-first source key (or testfn #'eq))))
    ('alist
     (if (lg-just-p maybe)
         (lg--alist-set-first-or-add source key (cdr maybe) (or testfn #'equal))
       (lg--alist-remove-first source key (or testfn #'equal))))
    ('hash-table
      (let ((copy (copy-hash-table source)))
       (if (lg-just-p maybe)
           (puthash key (cdr maybe) copy)
         (remhash key copy))
       copy))
    (_ (error "Unsupported keyed source type: %S" source))))

(defun lg-affine (preview-fn set-fn)
  "Build an affine traversal from PREVIEW-FN and SET-FN.
PREVIEW-FN must return tagged maybe (`lg-nothing' or `(lg-just . FOCUS)').
SET-FN is called as (SET-FN source new-focus)."
  (lg-traversal
   (lambda (afb source applicative)
     (let* ((result (funcall preview-fn source))
            (pure (lg-applicative-pure applicative))
            (fmap (lg-applicative-fmap applicative)))
       (if (lg-just-p result)
            (funcall fmap
                     (lambda (new-focus)
                       (funcall set-fn source new-focus))
                     (funcall afb (cdr result)))
          (funcall pure source))))))

(defalias 'lg--affine-traversal #'lg-affine)

(defun lg-ix (key &optional testfn)
  "Affine traversal focusing existing KEY in keyed SOURCE.
SOURCE can be plist, alist, or hash table.
TESTFN applies to plist/alist key comparisons."
  (lg-affine
   (lambda (source)
     (lg--keyed-present-and-value source key testfn))
    (lambda (source new-focus)
      (lg--keyed-set-existing source key new-focus testfn))))

(defun lg-ix-maybe (key &optional testfn)
  "Affine traversal equivalent to `lg-ix'.
Use with `lg-preview-maybe' to disambiguate missing vs present nil."
  (lg-ix key testfn))

(defconst lg-unmaybe
  (lg-lens
   #'lg-maybe-value
   (lambda (_maybe value)
      (if (null value) lg-nothing (lg-just value))))
  "Lossy maybe adapter lens.
Reading maps `lg-nothing' to nil and `(lg-just . VALUE)' to VALUE.
Writing maps nil to `lg-nothing' and non-nil values to `(lg-just . VALUE)'.")

(defun lg-at (key &optional testfn)
  "Lens focusing presence and value of KEY in keyed SOURCE.
The focus shape is tagged maybe (`lg-nothing' or `(lg-just . VALUE)').
Setting to `lg-nothing' removes KEY.
Setting to `(lg-just . VALUE)' inserts or updates KEY.
SOURCE can be plist, alist, or hash table.
TESTFN applies to plist/alist key comparisons."
  (lg-lens
   (lambda (source)
     (lg--keyed-present-and-value source key testfn))
   (lambda (source maybe)
     (lg--keyed-set-maybe source key maybe testfn))))

(defun lg-at-maybe (key &optional testfn)
  "Alias for `lg-at'."
  (lg-at key testfn))

(defun lg-hash-key (key)
  "Affine traversal focusing existing KEY in a hash table."
  (lg-ix key))

(defun lg-hash-key-at (key)
  "Lens focusing presence and value for KEY in a hash table."
  (lg-at key))

(defun lg-hash-key-at-maybe (key)
  "Lens focusing tagged maybe presence/value for KEY in a hash table."
  (lg-at-maybe key))

(defun lg-over (optic fn source)
  "Apply FN over OPTIC focus in SOURCE."
  (let* ((app (lg--identity-applicative))
         (profunctor (lg--star-profunctor app))
         (transform (lg--run-optic
                     optic
                     profunctor
                     (lambda (focus)
                       (make-lg-identity :value (funcall fn focus))))))
    (lg-identity-value (funcall transform source))))

(defun lg-iover (optic fn source)
  "Apply indexed FN over OPTIC focus in SOURCE.
FN is called as (FN index focus)."
  (let* ((app (lg--identity-applicative))
         (profunctor (lg--indexed-star-profunctor app))
         (transform (lg--run-indexed-optic
                     optic
                     profunctor
                     (lambda (index focus)
                       (make-lg-identity
                        :value (funcall fn index focus))))))
    (lg-identity-value (funcall transform source))))

(defun lg-set (optic value source)
  "Set OPTIC focus to VALUE in SOURCE."
  (lg-over optic (lambda (_focus) value) source))

(defun lg-foldl-of (optic fn initial source)
  "Left-fold OPTIC focuses in SOURCE with FN and INITIAL."
  (cl-reduce fn (lg-to-list-of optic source) :initial-value initial))

(defun lg-first-of (optic source)
  "Return first focus of OPTIC in SOURCE as tagged maybe."
  (lg-preview optic source))

(defun lg-last-of (optic source)
  "Return last focus of OPTIC in SOURCE as tagged maybe."
  (let ((focuses (lg-to-list-of optic source)))
    (if focuses
        (lg-just (car (last focuses)))
      lg-nothing)))

(defun lg-find-of (optic predicate source)
  "Return first focus matching PREDICATE for OPTIC in SOURCE as tagged maybe."
  (let* ((missing (make-symbol "lg-absent"))
         (found (cl-reduce (lambda (acc value)
                             (if (eq acc missing)
                                 (if (funcall predicate value) value missing)
                               acc))
                           (lg-to-list-of optic source)
                           :initial-value missing)))
    (if (eq found missing) lg-nothing (lg-just found))))

(defun lg-ifoldl-of (optic fn initial source)
  "Left-fold indexed OPTIC focuses in SOURCE with FN and INITIAL.
FN is called as (FN acc index focus)."
  (cl-reduce (lambda (acc indexed-focus)
               (funcall fn acc (car indexed-focus) (cdr indexed-focus)))
             (lg-ito-list-of optic source)
             :initial-value initial))

(defun lg-ifirst-of (optic source)
  "Return first indexed focus of OPTIC in SOURCE as tagged maybe."
  (lg-ipreview optic source))

(defun lg-ilast-of (optic source)
  "Return last indexed focus of OPTIC in SOURCE as tagged maybe."
  (let ((focuses (lg-ito-list-of optic source)))
    (if focuses
        (lg-just (car (last focuses)))
      lg-nothing)))

(defun lg-ifind-of (optic predicate source)
  "Return first indexed focus matching PREDICATE as tagged maybe.
PREDICATE is called as (PREDICATE index focus)."
  (let* ((missing (make-symbol "lg-absent"))
         (found (cl-reduce (lambda (acc indexed-focus)
                             (if (eq acc missing)
                                 (if (funcall predicate (car indexed-focus) (cdr indexed-focus))
                                     indexed-focus
                                   missing)
                               acc))
                           (lg-ito-list-of optic source)
                           :initial-value missing)))
    (if (eq found missing) lg-nothing (lg-just found))))

(defun lg-imap-of (optic fn source)
  "Indexed map over OPTIC in SOURCE using FN.
FN is called as (FN index focus)."
  (lg-iover optic fn source))

(defun lg-any-of (optic predicate source)
  "Return non-nil when any OPTIC focus in SOURCE satisfies PREDICATE."
  (cl-some predicate (lg-to-list-of optic source)))

(defun lg-none-of (optic predicate source)
  "Return non-nil when no OPTIC focus in SOURCE satisfies PREDICATE."
  (not (lg-any-of optic predicate source)))

(defun lg-all-of (optic predicate source)
  "Return non-nil when all OPTIC focuses in SOURCE satisfy PREDICATE."
  (cl-every predicate (lg-to-list-of optic source)))

(defun lg-iany-of (optic predicate source)
  "Return non-nil when any indexed focus satisfies PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (cl-some (lambda (indexed-focus)
             (funcall predicate (car indexed-focus) (cdr indexed-focus)))
           (lg-ito-list-of optic source)))

(defun lg-inone-of (optic predicate source)
  "Return non-nil when no indexed focus satisfies PREDICATE."
  (not (lg-iany-of optic predicate source)))

(defun lg-iall-of (optic predicate source)
  "Return non-nil when all indexed focuses satisfy PREDICATE.
PREDICATE is called as (PREDICATE index focus)."
  (cl-every (lambda (indexed-focus)
              (funcall predicate (car indexed-focus) (cdr indexed-focus)))
            (lg-ito-list-of optic source)))

(defun lg-count-of (optic source)
  "Count focused values of OPTIC in SOURCE."
  (length (lg-to-list-of optic source)))

(defun lg-icount-of (optic source)
  "Count focused values of indexed OPTIC in SOURCE."
  (length (lg-ito-list-of optic source)))

(defun lg-unto (builder)
  "Create a review optic from BUILDER.
BUILDER maps focus-domain values into source-domain values."
  (make-lg-optic
   :apply (lambda (_p pab) pab)
   :review builder))

(defun lg-reviews (optic fn value)
  "Review VALUE through OPTIC, then apply FN to the result."
  (funcall fn (lg-review optic value)))

(defun lg-preview-or (default optic source)
  "Preview OPTIC in SOURCE, returning DEFAULT only when missing."
  (let ((result (lg-preview optic source)))
    (if (lg-just-p result) (cdr result) default)))

(defun lg-to-list-of (optic source)
  "Collect all focus values for OPTIC in SOURCE."
  (let* ((monoid (make-lg-monoid :empty nil :append #'append))
         (app (lg--const-applicative monoid))
         (profunctor (lg--star-profunctor app))
         (transform (lg--run-optic
                     optic
                     profunctor
                     (lambda (focus)
                       (make-lg-const :value (list focus))))))
    (lg-const-value (funcall transform source))))

(defun lg-ito-list-of (optic source)
  "Collect all indexed focus values for OPTIC in SOURCE.
Each item is (INDEX . FOCUS)."
  (let* ((monoid (make-lg-monoid :empty nil :append #'append))
         (app (lg--const-applicative monoid))
         (profunctor (lg--indexed-star-profunctor app))
         (transform (lg--run-indexed-optic
                     optic
                     profunctor
                     (lambda (index focus)
                       (make-lg-const :value (list (cons index focus)))))))
    (lg-const-value (funcall transform source))))

(defun lg-ipreview (optic source)
  "Return a disambiguated indexed preview for OPTIC in SOURCE.
Returns tagged maybe containing (INDEX . VALUE)."
  (let ((focuses (lg-ito-list-of optic source)))
    (if focuses
        (lg-just (car focuses))
      lg-nothing)))

(defun lg-preview (optic source)
  "Return a disambiguated preview for OPTIC in SOURCE.
Returns tagged maybe (`lg-nothing' or `(lg-just . VALUE)').
VALUE can be nil when nil is an actual focus value."
  (let ((focuses (lg-to-list-of optic source)))
    (if focuses
        (lg-just (car focuses))
      lg-nothing)))

(defalias 'lg-ipreview-maybe #'lg-ipreview)
(defalias 'lg-preview-maybe #'lg-preview)

(defun lg-has (optic source)
  "Return non-nil when OPTIC has at least one focus in SOURCE."
  (lg-just-p (lg-preview optic source)))

(defun lg-view (optic source)
  "View exactly one expected focus from OPTIC in SOURCE.
Signals `lg-no-focus' when no focus exists."
  (let ((result (lg-preview optic source)))
    (if (lg-just-p result)
        (cdr result)
      (signal 'lg-no-focus (list optic source)))))

(defun lg-view-non-nil (optic source)
  "View focus with OPTIC in SOURCE and require a non-nil result."
  (let ((value (lg-view optic source)))
    (if value
        value
      (signal 'lg-expected-non-nil (list optic source)))))

(defmacro lg-optic (&rest optics)
  "Compose OPTICS at macro expansion time."
  `(lg-compose ,@optics))

(defconst lg-non-nil
  (lg-prism
   (lambda (value)
     (if value (lg-right value) (lg-left nil)))
   #'identity)
  "Prism that focuses a non-nil value.")

(defconst lg-car
  (lg-lens #'car (lambda (source new-focus) (cons new-focus (cdr source))))
  "Lens focusing car of a cons cell.")

(defconst lg-cdr
  (lg-lens #'cdr (lambda (source new-focus) (cons (car source) new-focus)))
  "Lens focusing cdr of a cons cell.")

(defconst lg-list
  (lg-traversal
   (lambda (afb source applicative)
     (lg--traverse-list applicative afb source)))
  "Traversal over all elements in a list.")

(defconst lg-indexed-list
  (lg-indexed
   (lambda (iafb source applicative)
     (lg--traverse-list-indexed applicative iafb source)))
  "Indexed traversal over all elements in a list.")

(defconst lg-vector
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                #'vconcat
                (lg--traverse-list applicative afb (append source nil))))))
  "Traversal over all elements in a vector.")

(defconst lg-string
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                (lambda (parts) (apply #'concat parts))
                (lg--traverse-list
                 applicative
                 (lambda (character)
                   (funcall afb (char-to-string character)))
                 (string-to-list source))))))
  "Traversal over all characters in a string.")

(defun lg-nth (index)
  "Lens focusing INDEX in a list or vector.
Signals when INDEX is out of range."
  (lg-lens
   (lambda (source)
      (unless (or (listp source) (vectorp source))
        (error "Expected list or vector source"))
      (unless (and (>= index 0) (< index (length source)))
        (error "Index %s out of range" index))
      (if (vectorp source)
          (aref source index)
        (nth index source)))
   (lambda (source new-focus)
      (unless (or (listp source) (vectorp source))
        (error "Expected list or vector source"))
      (unless (and (>= index 0) (< index (length source)))
        (error "Index %s out of range" index))
      (let ((result (copy-sequence source)))
        (if (vectorp result)
            (aset result index new-focus)
          (setf (nth index result) new-focus))
        result))))

(defun lg-plist-key (key &optional testfn)
  "Affine traversal focusing KEY in a plist.
TESTFN defaults to `eq'."
  (lg-ix key (or testfn #'eq)))

(defun lg-alist-key (key &optional testfn)
  "Affine traversal focusing KEY in an alist.
TESTFN defaults to `equal'."
  (lg-ix key (or testfn #'equal)))

(defconst lg-just-o
  (lg-prism
   (lambda (value)
     (if (and (consp value) (eq (car value) 'just))
         (lg-right (cdr value))
       (lg-left value)))
   (lambda (value)
     (cons 'just value)))
  "Prism for optional values represented as (just VALUE) or nil.")

(defconst lg-left-o
  (lg-prism
   (lambda (value)
     (if (lg-left-p value)
         (lg-right (cdr value))
       (lg-left value)))
   #'lg-left)
  "Prism focusing the left branch of a tagged either value.")

(defconst lg-right-o
  (lg-prism
   (lambda (value)
     (if (lg-right-p value)
         (lg-right (cdr value))
       (lg-left value)))
   #'lg-right)
  "Prism focusing the right branch of a tagged either value.")

(provide 'looking-glass)

;;; looking-glass.el ends here
