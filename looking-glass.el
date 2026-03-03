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

(cl-defstruct lg-state
  run)

(cl-defstruct lg-effect
  run)

(cl-defstruct lg-effect-op-view
  optic)

(cl-defstruct lg-effect-op-preview
  optic)

(cl-defstruct lg-effect-op-over
  optic
  fn)

(cl-defstruct lg-effect-op-set
  optic
  value)

(cl-defstruct lg-const
  value)

(cl-defstruct lg-forget
  run)

(cl-defstruct lg-tagged
  value)

(cl-defstruct lg-exchange
  sa
  bt)

(cl-defstruct lg-shop
  getter
  setter)

(cl-defstruct lg-market
  bt
  seta)

(cl-defstruct lg-stall
  sbt
  seta)

(cl-defstruct lg-bazaar
  run)

(cl-defstruct lg-rep-re
  run)

(cl-defstruct lg-profunctor
  dimap
  first
  right
  unfirst
  unright
  closed
  wander
  reindex)

(cl-defstruct lg-indexed
  run)

(cl-defstruct lg-optic
  apply)

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

(defconst lg-no-index 'lg-no-index
  "Sentinel index used when lifting unindexed optics.")

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

(defun lg-state-pure (value)
  "Return a state computation that yields VALUE without changing state."
  (make-lg-state :run (lambda (state)
                        (cons value state))))

(defun lg-state-fmap (fn st)
  "Map FN over ST result value."
  (make-lg-state
   :run (lambda (state)
          (let* ((result (funcall (lg-state-run st) state))
                 (value (car result))
                 (next-state (cdr result)))
            (cons (funcall fn value) next-state)))))

(defun lg-state-ap (sf st)
  "Apply stateful function SF to stateful argument ST."
  (lg-state-bind sf
                 (lambda (fn)
                   (lg-state-bind st
                                  (lambda (value)
                                    (lg-state-pure (funcall fn value)))))))

(defun lg-state-bind (st fn)
  "Sequence ST with FN, threading state.
FN is called with ST result and must return another state computation."
  (make-lg-state
   :run (lambda (state)
          (let* ((result (funcall (lg-state-run st) state))
                 (value (car result))
                 (next-state (cdr result))
                 (next-stateful (funcall fn value)))
            (funcall (lg-state-run next-stateful) next-state)))))

(defun lg-run-state (st initial-state)
  "Run state computation ST from INITIAL-STATE.
Returns (RESULT . FINAL-STATE)."
  (funcall (lg-state-run st) initial-state))

(defun lg-eval-state (st initial-state)
  "Run ST from INITIAL-STATE and return result value only."
  (car (lg-run-state st initial-state)))

(defun lg-exec-state (st initial-state)
  "Run ST from INITIAL-STATE and return final state only."
  (cdr (lg-run-state st initial-state)))

(defun lg-state-get ()
  "Return a state computation that yields current state."
  (make-lg-state :run (lambda (state)
                        (cons state state))))

(defun lg-state-put (new-state)
  "Return a state computation that replaces state with NEW-STATE."
  (make-lg-state :run (lambda (_state)
                        (cons nil new-state))))

(defun lg-state-modify (fn)
  "Return a state computation that transforms state with FN."
  (make-lg-state :run (lambda (state)
                        (cons nil (funcall fn state)))))

(defun lg-state-view-of (optic)
  "Return a state computation that views OPTIC from current state."
  (make-lg-state :run (lambda (state)
                        (cons (lg-view optic state) state))))

(defun lg-state-preview-of (optic)
  "Return a state computation that previews OPTIC from current state."
  (make-lg-state :run (lambda (state)
                        (cons (lg-preview optic state) state))))

(defun lg-state-over-of (optic fn)
  "Return a state computation that maps FN over OPTIC focus in state."
  (make-lg-state :run (lambda (state)
                        (cons nil (lg-over optic fn state)))))

(defun lg-state-set-of (optic value)
  "Return a state computation that sets OPTIC focus to VALUE."
  (lg-state-over-of optic (lambda (_focus) value)))

(defun lg-effect-pure (value)
  "Return an effect program that yields VALUE."
  (make-lg-effect :run (lambda (_context)
                         value)))

(defun lg-effect-map (fn effect)
  "Map FN over EFFECT result value."
  (make-lg-effect :run (lambda (context)
                         (funcall fn
                                  (funcall (lg-effect-run effect) context)))))

(defun lg-effect-bind (effect fn)
  "Sequence EFFECT with FN.
FN receives EFFECT result and must return another effect program." 
  (make-lg-effect
   :run (lambda (context)
          (let* ((value (funcall (lg-effect-run effect) context))
                 (next (funcall fn value)))
            (funcall (lg-effect-run next) context)))))

(defun lg-effect-ap (effect-fn effect-value)
  "Apply effectful function EFFECT-FN to EFFECT-VALUE."
  (lg-effect-bind effect-fn
                  (lambda (fn)
                    (lg-effect-bind effect-value
                                    (lambda (value)
                                      (lg-effect-pure (funcall fn value)))))))

(cl-defgeneric lg-effect-handle (context operation)
  "Handle OPERATION in CONTEXT.")

(cl-defmethod lg-effect-handle (context (operation lg-effect-op-view))
  (lg-view (lg-effect-op-view-optic operation) context))

(cl-defmethod lg-effect-handle (context (operation lg-effect-op-preview))
  (lg-preview (lg-effect-op-preview-optic operation) context))

(cl-defmethod lg-effect-handle (context (operation lg-effect-op-over))
  (lg-over (lg-effect-op-over-optic operation)
           (lg-effect-op-over-fn operation)
           context))

(cl-defmethod lg-effect-handle (context (operation lg-effect-op-set))
  (lg-set (lg-effect-op-set-optic operation)
          (lg-effect-op-set-value operation)
          context))

(defun lg-effect-perform (operation)
  "Lift OPERATION into an effect program.
Dispatch is provided by `lg-effect-handle'."
  (make-lg-effect :run (lambda (context)
                         (lg-effect-handle context operation))))

(defun lg-run-effect (effect context)
  "Run EFFECT program in CONTEXT and return its result."
  (funcall (lg-effect-run effect) context))

(defun lg-effect-view-of (optic)
  "Return an effectful view operation for OPTIC."
  (lg-effect-perform (make-lg-effect-op-view :optic optic)))

(defun lg-effect-preview-of (optic)
  "Return an effectful preview operation for OPTIC."
  (lg-effect-perform (make-lg-effect-op-preview :optic optic)))

(defun lg-effect-over-of (optic fn)
  "Return an effectful update operation applying FN over OPTIC."
  (lg-effect-perform (make-lg-effect-op-over :optic optic :fn fn)))

(defun lg-effect-set-of (optic value)
  "Return an effectful set operation for OPTIC and VALUE."
  (lg-effect-perform (make-lg-effect-op-set :optic optic :value value)))

(defun lg-view! (optic source)
  "View OPTIC focus in SOURCE.
This is an effect-marked synonym for `lg-view'."
  (lg-run-effect (lg-effect-view-of optic) source))

(defun lg-preview! (optic source)
  "Preview OPTIC focus in SOURCE.
This is an effect-marked synonym for `lg-preview'."
  (lg-run-effect (lg-effect-preview-of optic) source))

(defun lg-over! (optic fn source)
  "Apply FN over OPTIC focus in SOURCE.
For mutable sources this performs in-place mutation and returns SOURCE."
  (lg-run-effect (lg-effect-over-of optic fn) source))

(defun lg-set! (optic value source)
  "Set OPTIC focus to VALUE in SOURCE.
For mutable sources this performs in-place mutation and returns SOURCE."
  (lg-run-effect (lg-effect-set-of optic value) source))

(defconst lg-monoid-list
  (make-lg-monoid :empty nil :append #'append)
  "Monoid for list concatenation.")

(defconst lg-monoid-string
  (make-lg-monoid :empty "" :append #'concat)
  "Monoid for string concatenation.")

(defconst lg-monoid-sum
  (make-lg-monoid :empty 0 :append #'+)
  "Monoid for numeric summation.")

(defconst lg-monoid-product
  (make-lg-monoid :empty 1 :append #'*)
  "Monoid for numeric multiplication.")

(defconst lg-monoid-any
  (make-lg-monoid
   :empty nil
   :append (lambda (left right)
             (or left right)))
  "Monoid for logical disjunction.")

(defconst lg-monoid-all
  (make-lg-monoid
   :empty t
   :append (lambda (left right)
             (and left right)))
  "Monoid for logical conjunction.")

(defun lg--star-profunctor (applicative)
  "Return Star profunctor implementation over APPLICATIVE."
  (let ((fmap (lg-applicative-fmap applicative))
        (pure (lg-applicative-pure applicative)))
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
                  (funcall pure
                           (lg-left (cdr either))))))
     :unfirst (lambda (pab)
                (lambda (focus)
                  (funcall fmap
                           #'car
                           (funcall pab (cons focus nil)))))
     :unright (lambda (pab)
                (lambda (focus)
                  (funcall fmap
                           (lambda (either)
                             (if (lg-right-p either)
                                 (cdr either)
                               (error "Cochoice failed: expected right branch")))
                           (funcall pab (lg-right focus)))))
       :wander (lambda (wander-fn pab)
                 (lambda (value)
                   (funcall wander-fn pab value applicative))))))

(defun lg--identity-star-profunctor (applicative)
  "Return Star profunctor over identity APPLICATIVE with closed support."
  (let* ((base (lg--star-profunctor applicative))
         (dimap (lg-profunctor-dimap base))
         (first (lg-profunctor-first base))
         (right (lg-profunctor-right base))
         (unfirst (lg-profunctor-unfirst base))
         (unright (lg-profunctor-unright base))
         (wander (lg-profunctor-wander base)))
    (make-lg-profunctor
      :dimap dimap
      :first first
      :right right
      :unfirst unfirst
      :unright unright
      :wander wander
      :closed (lambda (pab)
                (lambda (function-source)
                 (make-lg-identity
                  :value
                  (lambda (argument)
                    (lg-identity-value
                     (funcall pab (funcall function-source argument))))))))))

(defun lg--indexed-star-profunctor (applicative)
  "Return Star profunctor for indexed optics over APPLICATIVE.
The mapping function receives (INDEX FOCUS)."
  (let ((fmap (lg-applicative-fmap applicative)))
    (make-lg-profunctor
     :dimap (lambda (before after indexed-pib)
              (make-lg-indexed
               :run (lambda (_index value)
                      (funcall fmap
                               after
                               (funcall (lg-indexed-run indexed-pib)
                                        lg-no-index
                                        (funcall before value))))))
     :first (lambda (indexed-pib)
              (make-lg-indexed
               :run (lambda (_index pair)
                      (let ((left (car pair))
                            (right (cdr pair)))
                        (funcall fmap
                                 (lambda (new-left) (cons new-left right))
                                 (funcall (lg-indexed-run indexed-pib)
                                          lg-no-index
                                          left))))))
     :right (lambda (indexed-pib)
              (make-lg-indexed
               :run (lambda (_index either)
                      (if (lg-right-p either)
                          (funcall fmap
                                   (lambda (new-right) (lg-right new-right))
                                   (funcall (lg-indexed-run indexed-pib)
                                            lg-no-index
                                            (cdr either)))
                        (funcall (lg-applicative-pure applicative)
                                 (lg-left (cdr either)))))))
     :wander (lambda (wander-fn indexed-pib)
               (make-lg-indexed
                :run (lambda (_index value)
                       (funcall wander-fn
                                (lambda (index focus)
                                  (funcall (lg-indexed-run indexed-pib)
                                           index
                                           focus))
                                value
                                applicative))))
      :reindex (lambda (index-fn indexed-pib)
                 (make-lg-indexed
                  :run (lambda (index focus)
                         (funcall (lg-indexed-run indexed-pib)
                                 (funcall index-fn index)
                                 focus)))))))

(defun lg--tagged-profunctor ()
  "Return Tagged profunctor implementation."
  (make-lg-profunctor
   :dimap (lambda (_before after tagged)
            (make-lg-tagged :value (funcall after (lg-tagged-value tagged))))
   :right (lambda (tagged)
            (make-lg-tagged :value (lg-right (lg-tagged-value tagged))))
   :unfirst (lambda (tagged)
              (let ((value (lg-tagged-value tagged)))
                (unless (consp value)
                  (error "Costrong failed: expected pair payload"))
                (make-lg-tagged :value (car value))))
   :unright (lambda (tagged)
              (let ((value (lg-tagged-value tagged)))
                (if (lg-right-p value)
                    (make-lg-tagged :value (cdr value))
                  (error "Cochoice failed: expected right payload"))))))

(defun lg--function-profunctor ()
  "Return profunctor implementation for plain functions." 
  (make-lg-profunctor
   :dimap (lambda (before after fn)
            (lambda (value)
              (funcall after (funcall fn (funcall before value)))))
   :first (lambda (fn)
            (lambda (pair)
              (cons (funcall fn (car pair)) (cdr pair))))
   :right (lambda (fn)
            (lambda (either)
              (if (lg-right-p either)
                  (lg-right (funcall fn (cdr either)))
                (lg-left (cdr either)))))
   :unfirst (lambda (fn)
              (lambda (focus)
                (car (funcall fn (cons focus nil)))))
   :unright (lambda (fn)
              (lambda (focus)
                (let ((result (funcall fn (lg-right focus))))
                  (if (lg-right-p result)
                      (cdr result)
                    (error "Cochoice failed: expected right branch")))))))

(defun lg--forget-profunctor (&optional empty)
  "Return Forget profunctor implementation.
When EMPTY is nil, nil is used as the empty value for left branches in Choice."
  (let ((mempty empty))
    (make-lg-profunctor
     :dimap (lambda (before _after forget)
              (make-lg-forget
               :run (lambda (value)
                      (funcall (lg-forget-run forget)
                               (funcall before value)))))
     :first (lambda (forget)
              (make-lg-forget
               :run (lambda (pair)
                      (funcall (lg-forget-run forget) (car pair)))))
     :right (lambda (forget)
              (make-lg-forget
               :run (lambda (either)
                      (if (lg-right-p either)
                          (funcall (lg-forget-run forget) (cdr either))
                        mempty))))
     :unfirst (lambda (forget)
                (make-lg-forget
                 :run (lambda (focus)
                        (funcall (lg-forget-run forget)
                                 (cons focus nil)))))
     :unright (lambda (forget)
                (make-lg-forget
                 :run (lambda (focus)
                        (funcall (lg-forget-run forget)
                                 (lg-right focus))))))))

(defun lg--exchange-profunctor ()
  "Return Exchange profunctor implementation."
  (make-lg-profunctor
   :dimap (lambda (before after exchange)
            (make-lg-exchange
             :sa (lambda (source)
                   (funcall (lg-exchange-sa exchange)
                            (funcall before source)))
             :bt (lambda (focus)
                   (funcall after
                            (funcall (lg-exchange-bt exchange) focus)))))))

(defun lg--shop-profunctor ()
  "Return Shop profunctor implementation."
  (make-lg-profunctor
   :dimap (lambda (before after shop)
            (make-lg-shop
             :getter (lambda (source)
                       (funcall (lg-shop-getter shop)
                                (funcall before source)))
             :setter (lambda (source focus)
                       (funcall after
                                (funcall (lg-shop-setter shop)
                                         (funcall before source)
                                         focus)))))
   :first (lambda (shop)
            (make-lg-shop
             :getter (lambda (pair)
                       (funcall (lg-shop-getter shop) (car pair)))
             :setter (lambda (pair focus)
                       (cons (funcall (lg-shop-setter shop)
                                      (car pair)
                                      focus)
                             (cdr pair)))))
   :right nil))

(defun lg--market-profunctor ()
  "Return Market profunctor implementation."
  (make-lg-profunctor
   :dimap (lambda (before after market)
            (make-lg-market
             :bt (lambda (focus)
                   (funcall after
                            (funcall (lg-market-bt market) focus)))
             :seta (lambda (source)
                     (let ((result (funcall (lg-market-seta market)
                                            (funcall before source))))
                       (if (lg-right-p result)
                           result
                         (lg-left (funcall after (cdr result))))))))
   :right (lambda (market)
            (make-lg-market
             :bt (lambda (focus)
                   (lg-right (funcall (lg-market-bt market) focus)))
             :seta (lambda (either)
                     (if (lg-right-p either)
                         (let ((inner (funcall (lg-market-seta market) (cdr either))))
                           (if (lg-right-p inner)
                               inner
                             (lg-left (lg-right (cdr inner)))))
                       (lg-left (lg-left (cdr either)))))))))

(defun lg--stall-profunctor ()
  "Return Stall profunctor implementation."
  (make-lg-profunctor
   :dimap (lambda (before after stall)
            (make-lg-stall
             :sbt (lambda (source focus)
                    (funcall after
                             (funcall (lg-stall-sbt stall)
                                      (funcall before source)
                                      focus)))
             :seta (lambda (source)
                     (let ((result (funcall (lg-stall-seta stall)
                                            (funcall before source))))
                       (if (lg-right-p result)
                           result
                         (lg-left (funcall after (cdr result))))))))
   :first (lambda (stall)
            (make-lg-stall
             :sbt (lambda (pair focus)
                    (cons (funcall (lg-stall-sbt stall) (car pair) focus)
                          (cdr pair)))
             :seta (lambda (pair)
                     (let ((result (funcall (lg-stall-seta stall) (car pair))))
                       (if (lg-right-p result)
                           result
                         (lg-left (cons (cdr result) (cdr pair))))))))
   :right (lambda (stall)
            (make-lg-stall
             :sbt (lambda (either focus)
                    (if (lg-right-p either)
                        (lg-right (funcall (lg-stall-sbt stall) (cdr either) focus))
                      (lg-left (cdr either))))
             :seta (lambda (either)
                     (if (lg-right-p either)
                         (let ((result (funcall (lg-stall-seta stall) (cdr either))))
                           (if (lg-right-p result)
                               result
                             (lg-left (lg-right (cdr result)))))
                        (lg-left (lg-left (cdr either)))))))))

(defun lg--re-profunctor (profunctor)
  "Return Re profunctor transformer over PROFUNCTOR."
  (let ((dimap (lg-profunctor-dimap profunctor))
        (unfirst (lg-profunctor-unfirst profunctor))
        (unright (lg-profunctor-unright profunctor)))
    (make-lg-profunctor
     :dimap (lambda (before after rep)
              (make-lg-rep-re
               :run (lambda (pdc)
                      (funcall (lg-rep-re-run rep)
                               (funcall dimap after before pdc)))))
     :first (and unfirst
                 (lambda (rep)
                   (make-lg-rep-re
                    :run (lambda (pbd)
                           (funcall (lg-rep-re-run rep)
                                    (funcall unfirst pbd))))))
     :right (and unright
                 (lambda (rep)
                   (make-lg-rep-re
                    :run (lambda (pbd)
                           (funcall (lg-rep-re-run rep)
                                    (funcall unright pbd)))))))))

(defun lg--run-optic (optic profunctor pab)
  "Run OPTIC against PROFUNCTOR using PAB."
  (funcall (lg-optic-apply optic) profunctor pab))

(defun lg--run-indexed-optic (optic profunctor indexed-pib)
  "Run indexed OPTIC against PROFUNCTOR using INDEXED-PIB."
  (funcall (lg-indexed-optic-apply optic) profunctor indexed-pib))

(defconst lg-id
  (make-lg-optic :apply (lambda (_p pab) pab))
  "Identity optic.")

(defconst lg-indexed-id
  (make-lg-indexed-optic :apply (lambda (_p indexed-pib) indexed-pib))
  "Identity indexed optic.")

(defun lg-compose2 (outer inner)
  "Compose OUTER with INNER.
The resulting optic focuses through INNER first, then OUTER."
  (make-lg-optic
   :apply (lambda (p pab)
            (lg--run-optic outer p (lg--run-optic inner p pab)))))

(defun lg-compose (&rest optics)
  "Compose OPTICS from right to left.
`(lg-compose o1 o2 o3)' means `o1' after `o2' after `o3'."
  (if optics
      (let ((result (car (last optics))))
        (dolist (optic (reverse (butlast optics)) result)
          (setq result (lg-compose2 optic result))))
    lg-id))

(defun lg<< (&rest optics)
  "Compose OPTICS from right to left.
This is syntactic sugar for `lg-compose'."
  (apply #'lg-compose optics))

(defun lg>> (&rest optics)
  "Compose OPTICS from left to right.
`(lg>> a b c)' is equivalent to `(lg-compose c b a)'."
  (apply #'lg-compose (reverse optics)))

(defun lg-compose-indexed2 (outer inner)
  "Compose indexed OUTER with indexed INNER."
  (make-lg-indexed-optic
   :apply (lambda (p pib)
            (lg--run-indexed-optic outer p (lg--run-indexed-optic inner p pib)))))

(defun lg-compose-indexed (&rest optics)
  "Compose indexed OPTICS from right to left."
  (if optics
      (let ((result (car (last optics))))
        (dolist (optic (reverse (butlast optics)) result)
          (setq result (lg-compose-indexed2 optic result))))
    lg-indexed-id))

(defun lg-iso (forward backward)
  "Build an isomorphism from FORWARD and BACKWARD."
  (make-lg-optic
   :apply (lambda (p pab)
            (funcall (lg-profunctor-dimap p) forward backward pab))))

;; Optic representation aliases (documentation only):
;; - Iso via Exchange (`lg-exchange`)
;; - Lens via Shop (`lg-shop`)
;; - Prism via Market (`lg-market`)
;; - AffineTraversal via Stall (`lg-stall`)
;; - Traversal via Wander/Bazaar-style interpretation (`lg-bazaar`)
;; - Fold/Getter via Forget (`lg-forget`)
;; - Review via Tagged (`lg-tagged`)

(defun lg-grate (builder)
  "Build a grate from BUILDER.
BUILDER is called as (BUILDER K), where K maps selectors (S -> A) to B."
  (make-lg-optic
   :apply (lambda (p pab)
            (let ((closed (lg-profunctor-closed p)))
              (unless closed
                (error "Grate requires profunctor closed"))
              (funcall (lg-profunctor-dimap p)
                       (lambda (source)
                         (lambda (selector)
                           (funcall selector source)))
                       builder
                       (funcall closed pab))))))

(defconst lg-unbool
  (lg-iso #'lg-bool-value #'lg-bool)
  "Iso between tagged lg booleans and Elisp t/nil booleans.")

(cl-defgeneric lg-not-value (value)
  "Return boolean negation of VALUE for supported boolean domains.
Supports tagged lg booleans (`lg-true'/`lg-false') and Elisp booleans (t/nil).")

(cl-defmethod lg-not-value ((value (eql lg-true)))
  lg-false)

(cl-defmethod lg-not-value ((value (eql lg-false)))
  lg-true)

(cl-defmethod lg-not-value ((value (eql t)))
  nil)

(cl-defmethod lg-not-value ((value (eql nil)))
  t)

(defconst lg-not
  (lg-iso #'lg-not-value #'lg-not-value)
  "Generic negation iso for lg booleans and Elisp booleans.")

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
                        (funcall right pab))))))

(defun lg-clone-iso (optic)
  "Clone OPTIC as an iso when possible."
  (let ((exchange (lg--run-optic
                   optic
                   (lg--exchange-profunctor)
                   (make-lg-exchange :sa #'identity :bt #'identity))))
    (lg-iso (lg-exchange-sa exchange)
            (lg-exchange-bt exchange))))

(defun lg-clone-lens (optic)
  "Clone OPTIC as a lens when possible."
  (let ((shop (lg--run-optic
               optic
               (lg--shop-profunctor)
               (make-lg-shop :getter #'identity
                             :setter (lambda (_source focus) focus)))))
    (lg-lens (lg-shop-getter shop)
             (lg-shop-setter shop))))

(defun lg-clone-prism (optic)
  "Clone OPTIC as a prism when possible."
  (let ((market (lg--run-optic
                 optic
                 (lg--market-profunctor)
                 (make-lg-market :bt #'identity
                                 :seta (lambda (source)
                                         (lg-right source))))))
    (lg-prism (lg-market-seta market)
              (lg-market-bt market))))

(defun lg-clone-affine (optic)
  "Clone OPTIC as an affine traversal when possible."
  (let ((stall (lg--run-optic
                optic
                (lg--stall-profunctor)
                (make-lg-stall :sbt (lambda (_source focus) focus)
                               :seta (lambda (source)
                                       (lg-right source))))))
    (lg-affine
     (lambda (source)
       (let ((result (funcall (lg-stall-seta stall) source)))
         (if (lg-right-p result)
             (lg-just (cdr result))
           lg-nothing)))
     (lg-stall-sbt stall))))

(defun lg-clone-traversal (optic)
  "Clone OPTIC as a traversal."
  (lg-traversal
   (lambda (afb source applicative)
     (let* ((profunctor (lg--star-profunctor applicative))
            (transform (lg--run-optic optic profunctor afb)))
       (funcall transform source)))))

(defun lg-clone-review (optic)
  "Clone OPTIC as a review when possible."
  (lg-unto (lambda (value)
             (lg-review optic value))))

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
  "Construct a target value from VALUE using OPTIC as a review.
Signals when OPTIC cannot be interpreted as a review."
  (let* ((profunctor (lg--tagged-profunctor))
         (result (condition-case nil
                     (lg--run-optic optic profunctor (make-lg-tagged :value value))
                   (error
                    (error "Optic does not support review")))))
    (lg-tagged-value result)))

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
   :apply (lambda (p indexed-pib)
            (let ((wander (lg-profunctor-wander p)))
              (unless wander
                (error "Indexed traversal requires indexed profunctor wander"))
              (funcall wander wander-fn indexed-pib)))))

(defun lg-ireindexed (index-fn optic)
  "Transform OPTIC indices with INDEX-FN."
  (make-lg-indexed-optic
   :apply (lambda (p indexed-pib)
            (let ((reindex (lg-profunctor-reindex p)))
              (unless reindex
                (error "Indexed optic requires indexed profunctor reindex"))
              (lg--run-indexed-optic optic p (funcall reindex index-fn indexed-pib))))))

(defun lg-as-indexed (optic)
  "Lift unindexed OPTIC into an indexed optic with `lg-no-index'."
  (lg-indexed
   (lambda (iafb source applicative)
      (let* ((profunctor (lg--star-profunctor applicative))
             (transform (lg--run-optic
                         optic
                         profunctor
                         (lambda (focus)
                           (funcall iafb lg-no-index focus)))))
        (funcall transform source)))))

(defun lg-unindexed (optic)
  "Drop index information from indexed OPTIC."
  (lg-traversal
   (lambda (afb source applicative)
     (let* ((profunctor (lg--indexed-star-profunctor applicative))
            (transform (lg--run-indexed-optic
                        optic
                        profunctor
                        (make-lg-indexed
                         :run (lambda (_index focus)
                                (funcall afb focus))))))
       (funcall (lg-indexed-run transform) lg-no-index source)))))

(defun lg-getter (getter-fn)
  "Build a read-only getter optic from GETTER-FN.
Setter-like operations leave the source unchanged."
  (lg-traversal
   (lambda (afb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                (lambda (_new-focus) source)
                (funcall afb (funcall getter-fn source)))))))

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
      (if (funcall predicate lg-no-index source)
          (funcall iafb lg-no-index source)
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
  (lg-compose optic lg-non-nil))

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

(defun lg--alist-p (value)
  "Return non-nil when VALUE is an alist."
  (and (listp value)
       (cl-every #'consp value)))

(defun lg--list-ix-kind (source)
  "Classify list SOURCE as plist/alist map-like container kind."
  (cond
   ((lg--alist-p source) 'alist)
   ((zerop (% (length source) 2)) 'plist)
   (t nil)))

(cl-defgeneric lg-ix-get (source key &optional testfn)
  "Return tagged maybe for existing KEY in SOURCE.
SOURCE extension point for `lg-ix' keyed reads.")

(cl-defgeneric lg-ix-set (source key value &optional testfn)
  "Set existing KEY to VALUE in SOURCE, preserving missing keys.
SOURCE extension point for `lg-ix' updates.")

(cl-defgeneric lg-at-get (source key &optional testfn)
  "Return tagged maybe presence/value for KEY in SOURCE.
SOURCE extension point for `lg-at' reads.")

(cl-defgeneric lg-at-set (source key maybe &optional testfn)
  "Set presence state for KEY in SOURCE.
SOURCE extension point for `lg-at' updates.
MAYBE must be `lg-nothing' or `(lg-just . VALUE)'.")

(cl-defgeneric lg--list-ix-get (kind source key &optional testfn)
  "List-backed map-like read dispatch by KIND.")

(cl-defgeneric lg--list-ix-set (kind source key value &optional testfn)
  "List-backed map-like existing-key update dispatch by KIND.")

(cl-defgeneric lg--list-at-set (kind source key maybe &optional testfn)
  "List-backed map-like maybe update dispatch by KIND.")

(cl-defmethod lg-ix-get ((source hash-table) key &optional _testfn)
  (let* ((missing (make-symbol "lg-absent"))
         (value (gethash key source missing)))
    (if (eq value missing)
        lg-nothing
      (lg-just value))))

(cl-defmethod lg-ix-get ((source list) key &optional testfn)
  (lg--list-ix-get (lg--list-ix-kind source) source key testfn))

(cl-defmethod lg-at-get (source key &optional testfn)
  (lg-ix-get source key testfn))

(cl-defmethod lg-ix-set ((source hash-table) key value &optional _testfn)
  (let* ((missing (make-symbol "lg-absent"))
         (current (gethash key source missing)))
    (if (eq current missing)
        source
      (let ((copy (copy-hash-table source)))
        (puthash key value copy)
        copy))))

(cl-defmethod lg-ix-set ((source list) key value &optional testfn)
  (lg--list-ix-set (lg--list-ix-kind source) source key value testfn))

(cl-defmethod lg-at-set (source _key maybe &optional _testfn)
  (unless (or (lg-nothing-p maybe) (lg-just-p maybe))
    (error "Expected tagged maybe value"))
  (cl-call-next-method))

(cl-defmethod lg-at-set ((source hash-table) key maybe &optional _testfn)
  (let ((copy (copy-hash-table source)))
    (if (lg-just-p maybe)
        (puthash key (cdr maybe) copy)
      (remhash key copy))
    copy))

(cl-defmethod lg-at-set ((source list) key maybe &optional testfn)
  (lg--list-at-set (lg--list-ix-kind source) source key maybe testfn))

(cl-defmethod lg--list-ix-get ((kind (eql plist)) source key &optional testfn)
  (let ((rest source)
        found
        value)
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (when (and (not found) (funcall (or testfn #'eq) candidate key))
          (setq found t
                value candidate-value))
        (setq rest (cddr rest))))
    (if found (lg-just value) lg-nothing)))

(cl-defmethod lg--list-ix-get ((kind (eql alist)) source key &optional testfn)
  (let ((cell (cl-find-if (lambda (entry)
                            (funcall (or testfn #'equal) (car entry) key))
                          source)))
    (if cell (lg-just (cdr cell)) lg-nothing)))

(cl-defmethod lg--list-ix-set ((kind (eql plist)) source key value &optional testfn)
  (let ((rest source)
        (result nil)
        (updated nil)
        (test (or testfn #'eq)))
    (while rest
      (let ((candidate (car rest))
            (candidate-value (cadr rest)))
        (if (and (not updated) (funcall test candidate key))
            (progn
              (setq updated t)
              (setq result (append result (list candidate value))))
          (setq result (append result (list candidate candidate-value)))))
      (setq rest (cddr rest)))
    result))

(cl-defmethod lg--list-ix-set ((kind (eql alist)) source key value &optional testfn)
  (let ((updated nil)
        (test (or testfn #'equal)))
    (mapcar (lambda (entry)
              (if (and (not updated) (funcall test (car entry) key))
                  (progn
                    (setq updated t)
                    (cons (car entry) value))
                entry))
            source)))

(cl-defmethod lg--list-at-set ((kind (eql plist)) source key maybe &optional testfn)
  (let ((test (or testfn #'eq)))
    (if (lg-just-p maybe)
        (if (lg-just-p (lg--list-ix-get kind source key test))
            (lg--list-ix-set kind source key (cdr maybe) test)
          (append source (list key (cdr maybe))))
      (let ((rest source)
            (result nil)
            (removed nil))
        (while rest
          (let ((candidate (car rest))
                (candidate-value (cadr rest)))
            (if (and (not removed) (funcall test candidate key))
                (setq removed t)
              (setq result (append result (list candidate candidate-value)))))
          (setq rest (cddr rest)))
        result))))

(cl-defmethod lg--list-at-set ((kind (eql alist)) source key maybe &optional testfn)
  (let ((test (or testfn #'equal)))
    (if (lg-just-p maybe)
        (if (lg-just-p (lg--list-ix-get kind source key test))
            (lg--list-ix-set kind source key (cdr maybe) test)
          (append source (list (cons key (cdr maybe)))))
      (let ((result nil)
            (removed nil))
        (dolist (entry source (nreverse result))
          (if (and (not removed) (funcall test (car entry) key))
              (setq removed t)
            (push entry result)))))))

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

(defun lg-ix (key &optional testfn)
  "Affine traversal focusing existing KEY in keyed SOURCE.
SOURCE can be plist, alist, or hash table.
TESTFN applies to plist/alist key comparisons."
  (lg-affine
   (lambda (source)
     (lg-ix-get source key testfn))
     (lambda (source new-focus)
       (lg-ix-set source key new-focus testfn))))

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
     (lg-at-get source key testfn))
   (lambda (source maybe)
     (lg-at-set source key maybe testfn))))

(defun lg-hash-key (key)
  "Affine traversal focusing existing KEY in a hash table."
  (lg-ix key))

(defun lg-hash-key-at (key)
  "Lens focusing presence and value for KEY in a hash table."
  (lg-at key))

(defun lg-over (optic fn source)
  "Apply FN over OPTIC focus in SOURCE."
  (let* ((app (lg--identity-applicative))
         (profunctor (lg--identity-star-profunctor app))
         (transform (lg--run-optic
                     optic
                     profunctor
                     (lambda (focus)
                        (make-lg-identity :value (funcall fn focus))))))
    (lg-identity-value (funcall transform source))))

(defun lg-map-of (optic fn source)
  "Map FN over OPTIC in SOURCE."
  (lg-over optic fn source))

(defun lg-for-of (optic source fn)
  "Map FN over OPTIC in SOURCE with source-first argument order."
  (lg-over optic fn source))

(defun lg-iover (optic fn source)
  "Apply indexed FN over OPTIC focus in SOURCE.
FN is called as (FN index focus)."
  (let* ((app (lg--identity-applicative))
         (profunctor (lg--indexed-star-profunctor app))
         (transform (lg--run-indexed-optic
                     optic
                     profunctor
                     (make-lg-indexed
                      :run (lambda (index focus)
                             (make-lg-identity
                              :value (funcall fn index focus)))))))
    (lg-identity-value (funcall (lg-indexed-run transform) lg-no-index source))))

(defun lg-imap-of (optic fn source)
  "Indexed map over OPTIC in SOURCE using FN.
FN is called as (FN index focus)."
  (lg-iover optic fn source))

(defun lg-ifor-of (optic source fn)
  "Indexed map over OPTIC in SOURCE with source-first order.
FN is called as (FN index focus)."
  (lg-iover optic fn source))

(defun lg-set (optic value source)
  "Set OPTIC focus to VALUE in SOURCE."
  (lg-over optic (lambda (_focus) value) source))

(defun lg-iset (optic value source)
  "Set indexed OPTIC focus to VALUE in SOURCE, ignoring indices."
  (lg-iover optic (lambda (_index _focus) value) source))

(defun lg-foldl-of (optic fn initial source)
  "Left-fold OPTIC focuses in SOURCE with FN and INITIAL."
  (cl-reduce fn (lg-to-list-of optic source) :initial-value initial))

(defun lg-foldr-of (optic fn initial source)
  "Right-fold OPTIC focuses in SOURCE with FN and INITIAL.
FN is called as (FN focus acc)."
  (let ((acc initial))
    (dolist (focus (reverse (lg-to-list-of optic source)) acc)
      (setq acc (funcall fn focus acc)))))

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

(defun lg-ifoldr-of (optic fn initial source)
  "Right-fold indexed OPTIC focuses in SOURCE with FN and INITIAL.
FN is called as (FN index focus acc)."
  (let ((acc initial))
    (dolist (indexed-focus (reverse (lg-ito-list-of optic source)) acc)
      (setq acc (funcall fn (car indexed-focus) (cdr indexed-focus) acc)))))

(defun lg-fold-map-of (optic monoid fn source)
  "Map each OPTIC focus with FN and combine using MONOID in SOURCE."
  (let* ((app (lg--const-applicative monoid))
         (profunctor (lg--star-profunctor app))
         (transform (lg--run-optic
                     optic
                     profunctor
                     (lambda (focus)
                       (make-lg-const :value (funcall fn focus))))))
    (lg-const-value (funcall transform source))))

(defun lg-ifold-map-of (optic monoid fn source)
  "Indexed fold-map over OPTIC in SOURCE using MONOID.
FN is called as (FN index focus)."
  (let* ((app (lg--const-applicative monoid))
         (profunctor (lg--indexed-star-profunctor app))
         (transform (lg--run-indexed-optic
                     optic
                     profunctor
                     (make-lg-indexed
                      :run (lambda (index focus)
                             (make-lg-const :value (funcall fn index focus)))))))
    (lg-const-value (funcall (lg-indexed-run transform) lg-no-index source))))

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

(defun lg-ipreview-or (default optic source)
  "Preview indexed OPTIC in SOURCE, returning DEFAULT only when missing."
  (let ((result (lg-ipreview optic source)))
    (if (lg-just-p result) (cdr result) default)))

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

(defun lg-length-of (optic source)
  "Return focus count of OPTIC in SOURCE."
  (length (lg-to-list-of optic source)))

(defun lg-ilength-of (optic source)
  "Return indexed focus count of OPTIC in SOURCE."
  (length (lg-ito-list-of optic source)))

(defun lg-sum-of (optic source)
  "Return numeric sum of OPTIC focuses in SOURCE."
  (lg-fold-map-of optic lg-monoid-sum #'identity source))

(defun lg-product-of (optic source)
  "Return numeric product of OPTIC focuses in SOURCE."
  (lg-fold-map-of optic lg-monoid-product #'identity source))

(defun lg-isum-of (optic source)
  "Return numeric sum of indexed OPTIC focuses in SOURCE."
  (lg-ifold-map-of optic lg-monoid-sum (lambda (_index focus) focus) source))

(defun lg-iproduct-of (optic source)
  "Return numeric product of indexed OPTIC focuses in SOURCE."
  (lg-ifold-map-of optic lg-monoid-product (lambda (_index focus) focus) source))

(defun lg-maximum-of (optic source)
  "Return maximum OPTIC focus in SOURCE as tagged maybe."
  (let ((focuses (lg-to-list-of optic source)))
    (if focuses
        (lg-just (cl-reduce #'max focuses))
      lg-nothing)))

(defun lg-minimum-of (optic source)
  "Return minimum OPTIC focus in SOURCE as tagged maybe."
  (let ((focuses (lg-to-list-of optic source)))
    (if focuses
        (lg-just (cl-reduce #'min focuses))
      lg-nothing)))

(defun lg-imaximum-of (optic source)
  "Return indexed maximum OPTIC focus in SOURCE as tagged maybe.
Result payload is (INDEX . VALUE)."
  (let ((focuses (lg-ito-list-of optic source)))
    (if focuses
        (lg-just (cl-reduce (lambda (best current)
                              (if (> (cdr current) (cdr best))
                                  current
                                best))
                            focuses))
      lg-nothing)))

(defun lg-iminimum-of (optic source)
  "Return indexed minimum OPTIC focus in SOURCE as tagged maybe.
Result payload is (INDEX . VALUE)."
  (let ((focuses (lg-ito-list-of optic source)))
    (if focuses
        (lg-just (cl-reduce (lambda (best current)
                              (if (< (cdr current) (cdr best))
                                  current
                                best))
                            focuses))
      lg-nothing)))

(defun lg-unto (builder)
  "Create a review optic from BUILDER.
BUILDER maps focus-domain values into source-domain values."
  (make-lg-optic
   :apply (lambda (p pab)
            (funcall (lg-profunctor-dimap p)
                     (lambda (_source)
                       (error "Cannot view through a pure review"))
                     builder
                     pab))))

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
                     (make-lg-indexed
                      :run (lambda (index focus)
                             (make-lg-const :value (list (cons index focus))))))))
    (lg-const-value (funcall (lg-indexed-run transform) lg-no-index source))))

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

(defun lg-ihas (optic source)
  "Return non-nil when indexed OPTIC has at least one focus in SOURCE."
  (lg-just-p (lg-ipreview optic source)))

(defun lg-has (optic source)
  "Return non-nil when OPTIC has at least one focus in SOURCE."
  (lg-just-p (lg-preview optic source)))

(defun lg-iview (optic source)
  "View exactly one indexed focus from OPTIC in SOURCE.
Signals `lg-no-focus' when no focus exists."
  (let ((result (lg-ipreview optic source)))
    (if (lg-just-p result)
        (cdr result)
      (signal 'lg-no-focus (list optic source)))))

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

(defun lg-iview-non-nil (optic source)
  "View indexed focus with OPTIC in SOURCE and require non-nil value."
  (let ((indexed-value (lg-iview optic source)))
    (if (cdr indexed-value)
        indexed-value
      (signal 'lg-expected-non-nil (list optic source)))))

(defun lg-re (optic)
  "Reverse OPTIC using Re profunctor interpretation.
Signals when OPTIC cannot be interpreted under Re constraints."
  (make-lg-optic
   :apply (lambda (p pba)
            (let* ((rep (condition-case nil
                            (lg--run-optic optic
                                           (lg--re-profunctor p)
                                           (make-lg-rep-re :run #'identity))
                          (error
                           (error "Optic cannot be reversed under current profunctor")))))
              (funcall (lg-rep-re-run rep) pba)))))

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

(defconst lg-indexed-vector
  (lg-indexed
   (lambda (iafb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                #'vconcat
                (lg--traverse-list-indexed
                 applicative
                 iafb
                 (append source nil))))))
  "Indexed traversal over all elements in a vector.")

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

(defconst lg-indexed-string
  (lg-indexed
   (lambda (iafb source applicative)
     (let ((fmap (lg-applicative-fmap applicative)))
       (funcall fmap
                (lambda (parts) (apply #'concat parts))
                (lg--traverse-list-indexed
                 applicative
                 (lambda (index character)
                   (funcall iafb index (char-to-string character)))
                 (string-to-list source))))))
  "Indexed traversal over all characters in a string.")

(cl-defgeneric lg-nth-get (source index)
  "Read INDEX from SOURCE for `lg-nth'.")

(cl-defgeneric lg-nth-set (source index new-focus)
  "Set INDEX in SOURCE to NEW-FOCUS for `lg-nth'.")

(cl-defmethod lg-nth-get ((source list) index)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (nth index source))

(cl-defmethod lg-nth-get ((source vector) index)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (aref source index))

(cl-defmethod lg-nth-get ((source string) index)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (aref source index))

(cl-defmethod lg-nth-get ((source bool-vector) index)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (aref source index))

(cl-defmethod lg-nth-set ((source list) index new-focus)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (let ((result (copy-sequence source)))
    (setf (nth index result) new-focus)
    result))

(cl-defmethod lg-nth-set ((source vector) index new-focus)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (let ((result (copy-sequence source)))
    (aset result index new-focus)
    result))

(cl-defmethod lg-nth-set ((source string) index new-focus)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (unless (characterp new-focus)
    (error "Expected character focus for string source"))
  (let ((result (copy-sequence source)))
    (aset result index new-focus)
    result))

(cl-defmethod lg-nth-set ((source bool-vector) index new-focus)
  (unless (and (>= index 0) (< index (length source)))
    (error "Index %s out of range" index))
  (unless (booleanp new-focus)
    (error "Expected boolean focus for bool-vector source"))
  (let ((result (copy-sequence source)))
    (aset result index new-focus)
    result))

(defun lg-nth (index)
  "Lens focusing INDEX in a list, vector, or string.
For strings, focus values are character codes.
Signals when INDEX is out of range."
  (lg-lens
   (lambda (source)
       (lg-nth-get source index))
   (lambda (source new-focus)
       (lg-nth-set source index new-focus))))

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
