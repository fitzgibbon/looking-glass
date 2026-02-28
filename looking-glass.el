;;; looking-glass.el --- Compiled profunctor optics for Emacs Lisp -*- lexical-binding: t; -*-

;; Author: looking-glass contributors
;; Version: 0.4.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.6"))
;; Keywords: lisp, data, tools
;; URL: https://github.com/fitzgibbon/looking-glass

;;; Commentary:

;; looking-glass uses profunctor encodings for both unindexed and indexed optics.
;;
;; - unindexed optics store REP: (PDICT PAB) -> PASBT
;; - indexed optics additionally store IREP: (IPDICT PAB) -> PASBT
;;
;; Public operations evaluate optics through REP/IREP directly.

;;; Code:

(require 'cl-lib)

(cl-defstruct (lg-applicative
               (:constructor lg--app-create))
  pure
  map2)

(cl-defstruct (lg-profunctor
               (:constructor lg--profunctor-create))
  dimap
  first
  right
  wander)

(cl-defstruct (lg-indexed-profunctor
               (:constructor lg--iprofunctor-create))
  dimap
  first
  right
  wander)

(cl-defstruct (lg-optic
               (:constructor lg--optic-create))
  kind
  cardinality
  caps
  indexed
  rep
  irep
  review-fn
  over-fn
  iover-fn
  set-fn
  view-fn
  preview-fn
  to-list-fn
  ito-list-fn
  plan)

(defvar lg--kind-registry (make-hash-table :test #'eq)
  "Registry of kind metadata.")

(defconst lg--either-left :left)
(defconst lg--either-right :right)

(defconst lg--app-identity
  (lg--app-create
   :pure (lambda (x) x)
   :map2 (lambda (f x y)
           (funcall f x y))))

(defconst lg--app-list-const
  (lg--app-create
   :pure (lambda (_x) nil)
   :map2 (lambda (_f xs ys)
           (append xs ys))))

(defconst lg--app-first-const
  (lg--app-create
   :pure (lambda (_x) nil)
   :map2 (lambda (_f x y)
           (or x y))))

(defun lg--app-map (app fn ax)
  "Map FN over applicative value AX under APP." 
  (funcall (lg-applicative-map2 app)
           (lambda (_ignore x) (funcall fn x))
           (funcall (lg-applicative-pure app) nil)
           ax))

(defconst lg--profunctor-fn
  (lg--profunctor-create
   :dimap (lambda (pre post pab)
            (lambda (source)
              (funcall post
                       (funcall pab
                                (funcall pre source)))))
   :first (lambda (pab)
            (lambda (pair)
              (cons (funcall pab (car pair))
                    (cdr pair))))
   :right (lambda (pab)
            (lambda (either)
              (if (eq (car either) lg--either-right)
                  (cons lg--either-right
                        (funcall pab (cdr either)))
                either)))
   :wander (lambda (traverse pab)
             (lambda (source)
               (funcall traverse pab source lg--app-identity)))))

(defconst lg--profunctor-forget-list
  (lg--profunctor-create
   :dimap (lambda (pre _post pab)
            (lambda (source)
              (funcall pab
                       (funcall pre source))))
   :first (lambda (pab)
            (lambda (pair)
              (funcall pab (car pair))))
   :right (lambda (pab)
            (lambda (either)
              (if (eq (car either) lg--either-right)
                  (funcall pab (cdr either))
                nil)))
   :wander (lambda (traverse pab)
             (lambda (source)
               (funcall traverse pab source lg--app-list-const)))))

(defconst lg--profunctor-forget-first
  (lg--profunctor-create
   :dimap (lambda (pre _post pab)
            (lambda (source)
              (funcall pab (funcall pre source))))
   :first (lambda (pab)
            (lambda (pair)
              (funcall pab (car pair))))
   :right (lambda (pab)
            (lambda (either)
              (if (eq (car either) lg--either-right)
                  (funcall pab (cdr either))
                nil)))
   :wander (lambda (traverse pab)
             (lambda (source)
               (funcall traverse pab source lg--app-first-const)))))

(defconst lg--iprofunctor-fn
  (lg--iprofunctor-create
   :dimap (lambda (pre post pab)
            (lambda (source)
              (funcall post
                       (funcall pab
                                (funcall pre source)))))
   :first (lambda (pab)
            (lambda (pair)
              (cons (funcall pab (car pair))
                    (cdr pair))))
   :right (lambda (pab)
            (lambda (either)
              (if (eq (car either) lg--either-right)
                  (cons lg--either-right
                        (funcall pab (cdr either)))
                either)))
   :wander (lambda (traverse pab)
             (lambda (source)
               (funcall traverse pab source lg--app-identity)))))

(defconst lg--iprofunctor-forget-list
  (lg--iprofunctor-create
   :dimap (lambda (pre _post pab)
            (lambda (source)
              (funcall pab (funcall pre source))))
   :first (lambda (pab)
            (lambda (pair)
              (funcall pab (car pair))))
   :right (lambda (pab)
            (lambda (either)
              (if (eq (car either) lg--either-right)
                  (funcall pab (cdr either))
                nil)))
   :wander (lambda (traverse pab)
             (lambda (source)
               (funcall traverse pab source lg--app-list-const)))))

(defun lg--ensure-optic (value)
  "Signal an error unless VALUE is an `lg-optic'."
  (unless (lg-optic-p value)
    (signal 'wrong-type-argument (list 'lg-optic-p value))))

(defun lg-register-kind (kind caps &optional cardinality)
  "Register KIND with CAPS and optional CARDINALITY."
  (puthash kind (list :caps (copy-sequence caps)
                      :cardinality cardinality)
           lg--kind-registry)
  kind)

(defun lg--register-default-kinds ()
  "Populate built-in kind metadata."
  (clrhash lg--kind-registry)
  (lg-register-kind 'iso '(view preview collect over set review) 'single)
  (lg-register-kind 'lens '(view preview collect over set) 'single)
  (lg-register-kind 'prism '(preview collect over review) 'optional)
  (lg-register-kind 'traversal '(preview collect over set) 'many)
  (lg-register-kind 'getter '(view preview collect) 'single)
  (lg-register-kind 'fold '(preview collect) 'many)
  (lg-register-kind 'setter '(over set) 'many)
  (lg-register-kind 'review '(review) 'single)
  (lg-register-kind 'indexed-lens '(view preview collect over set iover icollect) 'single)
  (lg-register-kind 'indexed-traversal '(preview collect over set iover icollect) 'many)
  t)

(defun lg--compose-cardinality (outer inner)
  "Compute composed cardinality from OUTER and INNER cardinalities."
  (pcase outer
    ('single inner)
    ('optional (pcase inner
                 ('single 'optional)
                 ('optional 'optional)
                 (_ 'many)))
    (_ 'many)))

(defun lg--combine-index (outer inner)
  "Combine OUTER and INNER index values for composed indexed optics."
  (cond
   ((and outer inner) (cons outer inner))
   (inner inner)
   (t outer)))

(defun lg--infer-kind (caps cardinality indexed)
  "Infer optic kind from CAPS, CARDINALITY, and INDEXED metadata."
  (let* ((has (lambda (cap) (memq cap caps)))
         (base
          (cond
           ((and (funcall has 'review)
                 (funcall has 'view)
                 (funcall has 'over)
                 (eq cardinality 'single))
            'iso)
           ((and (funcall has 'view)
                 (funcall has 'over)
                 (eq cardinality 'single))
            'lens)
           ((and (funcall has 'review)
                 (funcall has 'preview)
                 (funcall has 'over)
                 (eq cardinality 'optional))
            'prism)
           ((and (funcall has 'over)
                 (funcall has 'collect)
                 (eq cardinality 'many))
            'traversal)
           ((and (funcall has 'view)
                 (funcall has 'collect)
                 (not (funcall has 'over))
                 (eq cardinality 'single))
            'getter)
           ((and (funcall has 'collect)
                 (not (funcall has 'over)))
            'fold)
           ((and (funcall has 'over)
                 (not (funcall has 'collect)))
            'setter)
           ((funcall has 'review)
            'review)
           (t 'optic))))
    (if (and indexed
             (memq base '(lens traversal fold getter)))
        (intern (format "indexed-%s" base))
      base)))

(defun lg--compute-caps (optic)
  "Compute capability list from compiled slots in OPTIC."
  (let ((caps nil))
    (when (and (lg-optic-rep optic)
               (eq (lg-optic-cardinality optic) 'single))
      (push 'view caps))
    (when (lg-optic-rep optic) (push 'preview caps))
    (when (lg-optic-rep optic) (push 'collect caps))
    (when (lg-optic-rep optic) (push 'over caps))
    (when (lg-optic-rep optic) (push 'set caps))
    (when (lg-optic-review-fn optic) (push 'review caps))
    (when (lg-optic-irep optic) (push 'iover caps))
    (when (lg-optic-irep optic) (push 'icollect caps))
    (nreverse caps)))

(defun lg--lift-rep-to-irep (rep)
  "Lift unindexed REP into indexed representation with nil indices."
  (when rep
    (lambda (ipdict pab)
      (funcall rep
               lg--profunctor-fn
               (lambda (value)
                 (let ((result (funcall pab (cons nil value))))
                   (if (eq ipdict lg--iprofunctor-fn)
                       (cdr result)
                     result)))))))

(defun lg--effective-irep (optic)
  "Return indexed profunctor representation for OPTIC."
  (or (lg-optic-irep optic)
      (lg--lift-rep-to-irep (lg-optic-rep optic))))

(defun lg--finalize-optic (optic)
  "Fill derived metadata for OPTIC."
  (unless (lg-optic-irep optic)
    (setf (lg-optic-irep optic)
          (and (lg-optic-indexed optic)
               (lg--lift-rep-to-irep (lg-optic-rep optic)))))
  (let ((caps (lg--compute-caps optic)))
    (setf (lg-optic-caps optic) caps)
    (setf (lg-optic-kind optic)
          (lg--infer-kind caps
                          (lg-optic-cardinality optic)
                          (lg-optic-indexed optic))))
  optic)

(defun lg--make-optic (&rest args)
  "Create and finalize a raw optic from ARGS for `lg--optic-create'."
  (lg--finalize-optic (apply #'lg--optic-create args)))

(defun lg--compose-rep (outer inner)
  "Compose unindexed profunctor representations for OUTER then INNER."
  (let ((outer-rep (lg-optic-rep outer))
        (inner-rep (lg-optic-rep inner)))
    (when (and outer-rep inner-rep)
      (lambda (profunctor pab)
        (funcall outer-rep
                 profunctor
                 (funcall inner-rep profunctor pab))))))

(defun lg--compose-irep (outer inner)
  "Compose indexed profunctor representations for OUTER then INNER.

Composition combines indices as (OUTER . INNER)." 
  (let ((outer-irep (lg--effective-irep outer))
        (inner-irep (lg--effective-irep inner)))
    (when (and outer-irep inner-irep)
      (lambda (iprofunctor pab)
        (funcall outer-irep
                 iprofunctor
                (lambda (outer-idx+focus)
                  (let ((outer-index (car outer-idx+focus))
                        (outer-focus (cdr outer-idx+focus)))
                    (let ((inner-result
                           (funcall
                            (funcall inner-irep
                                     iprofunctor
                                     (lambda (inner-idx+value)
                                       (funcall pab
                                                (cons (lg--combine-index
                                                       outer-index
                                                       (car inner-idx+value))
                                                      (cdr inner-idx+value)))))
                            outer-focus)))
                      (if (eq iprofunctor lg--iprofunctor-fn)
                          (cons outer-index inner-result)
                        inner-result)))))))))

(defun lg--compose-review (outer inner)
  "Compose review path for OUTER then INNER." 
  (let ((outer-review (lg-optic-review-fn outer))
        (inner-review (lg-optic-review-fn inner)))
    (when (and outer-review inner-review)
      (lambda (value)
        (funcall outer-review (funcall inner-review value))))))

(defun lg--compose2 (outer inner)
  "Compile composition OUTER then INNER into one optic."
  (let* ((cardinality (lg--compose-cardinality
                       (lg-optic-cardinality outer)
                       (lg-optic-cardinality inner)))
         (indexed-chain (or (lg-optic-indexed outer)
                            (lg-optic-indexed inner)))
         (rep (lg--compose-rep outer inner))
         (irep (and indexed-chain (lg--compose-irep outer inner)))
         (optic
          (lg--optic-create
           :kind 'optic
           :cardinality cardinality
           :indexed indexed-chain
           :rep rep
            :irep irep
            :review-fn (lg--compose-review outer inner)
           :plan (append (or (lg-optic-plan outer)
                             (list (lg-optic-kind outer)))
                         (or (lg-optic-plan inner)
                             (list (lg-optic-kind inner)))))))
    (lg--finalize-optic optic)))

(defun lg-compose (&rest optics)
  "Compose OPTICS left-to-right.

Composition combines profunctor representations and compiles operation paths."
  (if (null optics)
      (lg-id)
    (progn
      (mapc #'lg--ensure-optic optics)
      (let ((compiled (car optics)))
        (dolist (optic (cdr optics))
          (setq compiled (lg--compose2 compiled optic)))
        compiled))))

(defun lg-kind (optic)
  "Return inferred kind symbol for OPTIC."
  (lg--ensure-optic optic)
  (lg-optic-kind optic))

(defun lg-capabilities (optic)
  "Return supported capability symbols for OPTIC."
  (lg--ensure-optic optic)
  (copy-sequence (lg-optic-caps optic)))

(defun lg-can-p (optic capability)
  "Return non-nil when OPTIC supports CAPABILITY."
  (lg--ensure-optic optic)
  (memq capability (lg-optic-caps optic)))

(defun lg-over (optic fn source)
  "Apply FN to every focus selected by OPTIC in SOURCE."
  (lg--ensure-optic optic)
  (let ((rep (lg-optic-rep optic)))
    (unless rep
      (error "Optic kind %S cannot perform `lg-over'" (lg-kind optic)))
    (funcall (funcall rep lg--profunctor-fn fn) source)))

(defun lg-iover (optic fn source)
  "Apply indexed FN to every focus selected by OPTIC in SOURCE.

FN receives INDEX and VALUE."
  (lg--ensure-optic optic)
  (let ((irep (lg--effective-irep optic)))
    (unless irep
      (error "Optic kind %S cannot perform `lg-iover'" (lg-kind optic)))
    (funcall (funcall irep lg--iprofunctor-fn
                      (lambda (idx+value)
                        (cons (car idx+value)
                              (funcall fn (car idx+value)
                                       (cdr idx+value)))))
             source)))

(defun lg-set (optic value source)
  "Set every focus selected by OPTIC in SOURCE to VALUE."
  (lg-over optic (lambda (_focus) value) source))

(defun lg-to-list-of (optic source)
  "Collect all focuses selected by OPTIC in SOURCE."
  (lg--ensure-optic optic)
  (let ((rep (lg-optic-rep optic)))
    (unless rep
      (error "Optic kind %S cannot perform `lg-to-list-of'" (lg-kind optic)))
    (funcall (funcall rep lg--profunctor-forget-list
                      (lambda (v) (list v)))
             source)))

(defun lg-ito-list-of (optic source)
  "Collect all indexed focuses selected by OPTIC in SOURCE.

Return a list of (INDEX . VALUE) pairs."
  (lg--ensure-optic optic)
  (let ((irep (lg--effective-irep optic)))
    (unless irep
      (error "Optic kind %S cannot perform `lg-ito-list-of'" (lg-kind optic)))
    (let ((raw (funcall (funcall irep lg--iprofunctor-forget-list
                                 (lambda (idx+value) (list idx+value)))
                        source)))
      (mapcar (lambda (entry)
                (if (and (consp entry)
                         (null (cdr entry))
                         (consp (car entry)))
                    (car entry)
                  entry))
              raw))))

(defun lg-view (optic source)
  "Extract exactly one focus from OPTIC in SOURCE."
  (let ((foci (lg-to-list-of optic source)))
    (pcase foci
      (`(,value) value)
      (_ (error "`lg-view' expected exactly one focus, got %d"
                (length foci))))))

(defun lg-preview (optic source)
  "Extract the first focus from OPTIC in SOURCE, or nil."
  (lg--ensure-optic optic)
  (let ((rep (lg-optic-rep optic)))
    (unless rep
      (error "Optic kind %S cannot perform `lg-preview'" (lg-kind optic)))
    (funcall (funcall rep lg--profunctor-forget-first
                      (lambda (value) value))
             source)))

(defun lg-review (optic value)
  "Construct a source value by reviewing VALUE with OPTIC."
  (lg--ensure-optic optic)
  (let ((op (lg-optic-review-fn optic)))
    (unless op
      (error "Optic kind %S cannot perform `lg-review'" (lg-kind optic)))
    (funcall op value)))

(defun lg-lens (getter setter)
  "Create a lens from GETTER and SETTER.

GETTER is (SOURCE) -> FOCUS.
SETTER is (FOCUS SOURCE) -> SOURCE."
  (lg--make-optic
   :kind 'lens
   :cardinality 'single
   :rep (lambda (pdict pab)
          (let ((dimap (lg-profunctor-dimap pdict))
                (first (lg-profunctor-first pdict)))
            (funcall dimap
                     (lambda (source)
                       (cons (funcall getter source) source))
                     (lambda (pair)
                       (funcall setter (car pair) (cdr pair)))
                     (funcall first pab))))))

(defun lg-ilens (getter setter indexer)
  "Create an indexed lens from GETTER, SETTER, and INDEXER.

INDEXER is (SOURCE) -> INDEX."
  (lg--make-optic
   :kind 'indexed-lens
   :cardinality 'single
   :indexed t
   :rep (lambda (pdict pab)
          (let ((dimap (lg-profunctor-dimap pdict))
                (first (lg-profunctor-first pdict)))
            (funcall dimap
                     (lambda (source)
                       (cons (funcall getter source) source))
                     (lambda (pair)
                       (funcall setter (car pair) (cdr pair)))
                     (funcall first pab))))
   :irep (lambda (ipdict pab)
           (let ((dimap (lg-indexed-profunctor-dimap ipdict))
                 (first (lg-indexed-profunctor-first ipdict)))
             (funcall dimap
                      (lambda (source)
                        (cons (cons (funcall indexer source)
                                    (funcall getter source))
                              source))
                      (lambda (pair)
                        (funcall setter (cdr (car pair)) (cdr pair)))
                      (funcall first pab))))))

(defun lg-prism (matcher reviewer)
  "Create a prism from MATCHER and REVIEWER.

MATCHER is (SOURCE) -> nil or a cons whose cdr is focus.
REVIEWER is (FOCUS) -> SOURCE."
  (lg--make-optic
   :kind 'prism
   :cardinality 'optional
   :review-fn reviewer
   :rep (lambda (pdict pab)
          (let ((dimap (lg-profunctor-dimap pdict))
                (right (lg-profunctor-right pdict)))
            (funcall dimap
                     (lambda (source)
                       (let ((match (funcall matcher source)))
                         (if (and (consp match) (car match))
                             (cons lg--either-right (cdr match))
                           (cons lg--either-left source))))
                     (lambda (either)
                       (if (eq (car either) lg--either-right)
                           (funcall reviewer (cdr either))
                         (cdr either)))
                     (funcall right pab))))))

(defun lg-wander (traverse)
  "Create a traversal from TRAVERSE using profunctor `wander'.

TRAVERSE is (STEP SOURCE APP) -> APP-RESULT where APP is an `lg-applicative'."
  (lg--make-optic
   :kind 'traversal
   :cardinality 'many
   :rep (lambda (pdict pab)
          (funcall (lg-profunctor-wander pdict) traverse pab))))

(defun lg-iwander (traverse)
  "Create an indexed traversal from TRAVERSE using indexed profunctor `wander'.

TRAVERSE is (STEP SOURCE APP) -> APP-RESULT where STEP takes one
indexed pair (INDEX . VALUE) and returns an applicative indexed pair." 
  (lg--make-optic
   :kind 'indexed-traversal
   :cardinality 'many
   :indexed t
   :rep (lambda (pdict pab)
          (funcall (lg-profunctor-wander pdict)
                   (lambda (step source app)
                     (funcall traverse
                              (lambda (idx+value)
                                (lg--app-map app #'cdr (funcall step idx+value)))
                              source app))
                   pab))
   :irep (lambda (ipdict pab)
           (funcall (lg-indexed-profunctor-wander ipdict) traverse pab))))

(defun lg-traversal (over to-list)
  "Create a traversal from OVER and TO-LIST.

This is an adapter constructor; use `lg-wander' for explicit profunctor style."
  (lg--make-optic
   :kind 'traversal
   :cardinality 'many
   :over-fn over
   :to-list-fn to-list))

(defun lg-itraversal (iover ito-list)
  "Create an indexed traversal from IOVER and ITO-LIST.

This is an adapter constructor.
Use `lg-iwander' for explicit indexed profunctor style."
  (lg--make-optic
   :kind 'indexed-traversal
   :cardinality 'many
   :indexed t
   :iover-fn iover
   :ito-list-fn ito-list
   :over-fn (lambda (fn source)
              (funcall iover (lambda (_index value) (funcall fn value)) source))
   :to-list-fn (lambda (source)
                 (mapcar #'cdr (funcall ito-list source)))))

(defun lg-filtered (pred)
  "Traversal that keeps values where PRED returns non-nil."
  (lg-wander
   (lambda (step source app)
     (if (funcall pred source)
         (funcall step source)
       (funcall (lg-applicative-pure app) source)))))

(defun lg-ifiltered (pred)
  "Indexed traversal that keeps focuses where PRED INDEX VALUE is non-nil."
  (lg-iwander
   (lambda (step idx+value app)
     (if (funcall pred (car idx+value) (cdr idx+value))
         (funcall step idx+value)
       (funcall (lg-applicative-pure app) idx+value)))))

(defun lg-iso (forward backward)
  "Create an iso optic from FORWARD and BACKWARD."
  (lg--make-optic
   :kind 'iso
   :cardinality 'single
   :review-fn backward
   :rep (lambda (pdict pab)
          (funcall (lg-profunctor-dimap pdict)
                   forward
                   backward
                   pab))))

(defun lg-getter (getter)
  "Create a getter optic from GETTER." 
  (lg--make-optic
   :kind 'getter
   :cardinality 'single
   :view-fn getter
   :preview-fn getter
   :to-list-fn (lambda (source)
                 (list (funcall getter source)))))

(defun lg-igetter (getter)
  "Create an indexed getter from GETTER.

GETTER returns a single (INDEX . VALUE) pair."
  (lg-iwander
   (lambda (step source app)
     (lg--app-map app
                  (lambda (_idx+value) source)
                  (funcall step (funcall getter source))))))

(defun lg-fold (collector)
  "Create a fold optic from COLLECTOR.

COLLECTOR is (SOURCE) -> list of values." 
  (lg--make-optic
   :kind 'fold
   :cardinality 'many
   :preview-fn (lambda (source)
                 (car (funcall collector source)))
   :to-list-fn collector))

(defun lg-ifold (collector)
  "Create an indexed fold from COLLECTOR.

COLLECTOR returns a list of (INDEX . VALUE) pairs."
  (lg-iwander
   (lambda (step source app)
     (let* ((pure (lg-applicative-pure app))
            (map2 (lg-applicative-map2 app))
            (acc (funcall pure nil)))
       (dolist (idx+value (funcall collector source))
         (setq acc
               (funcall map2
                        (lambda (xs x)
                          (append xs (list x)))
                        acc
                        (funcall step idx+value))))
       (lg--app-map app (lambda (_updated) source) acc)))))

(defun lg-setter (over)
  "Create a setter optic from OVER.

OVER is (FN SOURCE) -> SOURCE." 
  (lg--make-optic
   :kind 'setter
   :cardinality 'many
   :over-fn over))

(defun lg-isetter (iover)
  "Create an indexed setter from IOVER.

IOVER is (FN SOURCE) -> SOURCE where FN receives INDEX and VALUE."
  (lg-iwander
   (lambda (step source app)
     (let ((pure (lg-applicative-pure app))
           (map2 (lg-applicative-map2 app)))
       (funcall map2
                (lambda (_left right) right)
                (funcall pure nil)
                (funcall pure
                         (funcall iover
                                  (lambda (idx value)
                                    (cdr (funcall step (cons idx value))))
                                  source)))))))

(defun lg-reviewer (review)
  "Create a review-only optic from REVIEW.

REVIEW is (VALUE) -> SOURCE." 
  (lg--make-optic
   :kind 'review
   :cardinality 'single
   :review-fn review))

(defun lg-id ()
  "Identity optic."
  (lg-iso #'identity #'identity))

(defun lg--list-traverse (step source app)
  "Traverse SOURCE list with STEP under APP applicative." 
  (let ((pure (lg-applicative-pure app))
        (map2 (lg-applicative-map2 app))
        (acc nil))
    (setq acc (funcall pure nil))
    (dolist (value source acc)
      (setq acc
            (funcall map2
                     (lambda (xs y)
                       (append xs (list y)))
                     acc
                     (funcall step value))))))

(defun lg--ilist-traverse (step source app)
  "Indexed traversal for SOURCE list with indexed STEP under APP." 
  (let ((pure (lg-applicative-pure app))
        (map2 (lg-applicative-map2 app))
        (acc nil)
        (index 0))
    (setq acc (funcall pure nil))
    (dolist (value source acc)
      (let ((idx index))
        (setq acc
              (funcall map2
                       (lambda (xs idx+value)
                         (append xs (list (cdr idx+value))))
                       acc
                       (funcall step (cons idx value)))))
      (setq index (1+ index)))))

(defun lg--regex-step (regexp string start)
  "Return match information for REGEXP in STRING at START.

Return nil when no match exists, or (MATCH-START MATCH-END NEXT-START)."
  (when (string-match regexp string start)
    (let* ((mstart (match-beginning 0))
           (mend (match-end 0))
           (next (if (= start mend)
                     (min (1+ start) (length string))
                   mend)))
      (list mstart mend next))))

(defun lg--regex-traverse-builder (regexp group)
  "Return unindexed TRAVERSE function for REGEXP and GROUP." 
  (lambda (step source app)
    (unless (stringp source)
      (error "`lg-regex' expects SOURCE to be a string"))
    (let ((pure (lg-applicative-pure app))
          (map2 (lg-applicative-map2 app))
          (scan 0)
          (cursor 0)
          (acc nil))
      (setq acc (funcall pure ""))
      (while (< scan (length source))
        (let ((step-info (lg--regex-step regexp source scan)))
          (if (null step-info)
              (progn
                (setq acc (funcall map2 #'concat acc
                                   (funcall pure (substring source cursor))))
                (setq scan (length source)))
            (pcase-let ((`(,_mstart ,mend ,next) step-info))
              (let ((gstart (match-beginning group))
                    (gend (match-end group)))
                (if (or (null gstart) (null gend))
                    (progn
                      (setq acc (funcall map2 #'concat acc
                                         (funcall pure (substring source cursor mend))))
                      (setq cursor mend))
                  (setq acc (funcall map2 #'concat acc
                                     (funcall pure (substring source cursor gstart))))
                  (setq acc (funcall map2 #'concat acc
                                     (funcall step (substring source gstart gend))))
                  (setq acc (funcall map2 #'concat acc
                                     (funcall pure (substring source gend mend))))
                  (setq cursor mend)))
              (setq scan next)))))
      acc)))

(defun lg--iregex-traverse-builder (regexp group)
  "Return indexed TRAVERSE function for REGEXP and GROUP.

Indices are zero-based match ordinals for matches where GROUP is present."
  (lambda (step source app)
    (unless (stringp source)
      (error "`lg-iregex' expects SOURCE to be a string"))
    (let ((pure (lg-applicative-pure app))
          (map2 (lg-applicative-map2 app))
          (scan 0)
          (cursor 0)
          (match-index 0)
          (acc nil))
      (setq acc (funcall pure ""))
      (while (< scan (length source))
        (let ((step-info (lg--regex-step regexp source scan)))
          (if (null step-info)
              (progn
                (setq acc (funcall map2 #'concat acc
                                   (funcall pure (substring source cursor))))
                (setq scan (length source)))
            (pcase-let ((`(,_mstart ,mend ,next) step-info))
              (let ((gstart (match-beginning group))
                    (gend (match-end group)))
                (if (or (null gstart) (null gend))
                    (progn
                      (setq acc (funcall map2 #'concat acc
                                         (funcall pure (substring source cursor mend))))
                      (setq cursor mend))
                  (let ((idx match-index))
                    (setq acc (funcall map2 #'concat acc
                                       (funcall pure (substring source cursor gstart))))
                    (setq acc
                          (funcall map2 #'concat acc
                                   (lg--app-map app #'cdr
                                                (funcall step
                                                         (cons idx
                                                               (substring source gstart gend))))))
                    (setq acc (funcall map2 #'concat acc
                                       (funcall pure (substring source gend mend))))
                    (setq cursor mend)
                    (setq match-index (1+ match-index)))))
              (setq scan next)))))
      acc)))

(defun lg-nth (index)
  "Lens focusing INDEX in a proper list."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-lens
   (lambda (source)
     (nth index source))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (setf (nth index copy) new)
       copy))))

(defun lg-inth (index)
  "Indexed lens focusing INDEX in a proper list."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-ilens
   (lambda (source)
     (nth index source))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (setf (nth index copy) new)
       copy))
   (lambda (_source) index)))

(defun lg-string-nth (index)
  "Lens focusing INDEX in a string."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-lens
   (lambda (source)
     (aref source index))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (aset copy index new)
       copy))))

(defun lg-istring-nth (index)
  "Indexed lens focusing INDEX in a string."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-ilens
   (lambda (source)
     (aref source index))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (aset copy index new)
       copy))
   (lambda (_source) index)))

(defun lg-bool-vector-nth (index)
  "Lens focusing INDEX in a bool-vector."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-lens
   (lambda (source)
     (aref source index))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (aset copy index new)
       copy))))

(defun lg-ibool-vector-nth (index)
  "Indexed lens focusing INDEX in a bool-vector."
  (unless (and (integerp index) (>= index 0))
    (error "INDEX must be a non-negative integer"))
  (lg-ilens
   (lambda (source)
     (aref source index))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (aset copy index new)
       copy))
   (lambda (_source) index)))

(defun lg-char-table-char (char)
  "Lens focusing CHAR in a char-table."
  (lg-lens
   (lambda (source)
     (aref source char))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (aset copy char new)
       copy))))

(defun lg-ichar-table-char (char)
  "Indexed lens focusing CHAR in a char-table."
  (lg-ilens
   (lambda (source)
     (aref source char))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (aset copy char new)
       copy))
   (lambda (_source) char)))

(defun lg--alist-match-p (key cell test)
  "Return non-nil when CELL key matches KEY under TEST."
  (and (consp cell)
       (funcall (or test #'equal) key (car cell))))

(defun lg--alist-get (key source test)
  "Return value for first KEY match in SOURCE under TEST."
  (let ((found nil)
        (value nil))
    (while (and source (not found))
      (let ((cell (car source)))
        (when (lg--alist-match-p key cell test)
          (setq value (cdr cell))
          (setq found t)))
      (setq source (cdr source)))
    value))

(defun lg-alist (key &optional test)
  "Lens focusing first KEY match in an alist.

When KEY is not present, setting inserts it at the front."
  (lg-lens
   (lambda (source)
     (lg--alist-get key source test))
   (lambda (new source)
     (let ((out nil)
           (done nil))
       (dolist (cell source)
         (if (and (not done) (lg--alist-match-p key cell test))
             (progn
               (push (cons key new) out)
               (setq done t))
           (push cell out)))
       (unless done
         (push (cons key new) out))
       (nreverse out)))))

(defun lg-ialist (key &optional test)
  "Indexed lens focusing first KEY match in an alist.

Index for the focus is KEY."
  (lg-ilens
   (lambda (source)
     (lg--alist-get key source test))
   (lambda (new source)
     (let ((out nil)
           (done nil))
       (dolist (cell source)
         (if (and (not done) (lg--alist-match-p key cell test))
             (progn
               (push (cons key new) out)
               (setq done t))
           (push cell out)))
       (unless done
         (push (cons key new) out))
       (nreverse out)))
   (lambda (_source) key)))

(defun lg-alist-values (key &optional test)
  "Traversal over all values whose key matches KEY in an alist."
  (lg-wander
   (lambda (step source app)
     (let* ((pure (lg-applicative-pure app))
            (map2 (lg-applicative-map2 app))
            (acc (funcall pure nil)))
       (dolist (cell source acc)
         (if (lg--alist-match-p key cell test)
             (setq acc
                   (funcall map2
                            (lambda (cells value)
                              (append cells (list (cons (car cell) value))))
                            acc
                            (funcall step (cdr cell))))
           (setq acc
                 (funcall map2
                          (lambda (cells c) (append cells (list c)))
                          acc
                          (funcall pure cell)))))))))

(defun lg-ialist-values (key &optional test)
  "Indexed traversal over all values whose key matches KEY in an alist.

Index for each focus is KEY."
  (lg-iwander
   (lambda (step source app)
     (let* ((pure (lg-applicative-pure app))
            (map2 (lg-applicative-map2 app))
            (acc (funcall pure nil)))
       (dolist (cell source acc)
         (if (lg--alist-match-p key cell test)
             (setq acc
                   (funcall map2
                            (lambda (cells idx+value)
                              (append cells (list (cons (car cell) (cdr idx+value)))))
                            acc
                            (funcall step (cons key (cdr cell)))))
           (setq acc
                 (funcall map2
                          (lambda (cells c) (append cells (list c)))
                          acc
                          (funcall pure cell)))))))))

(defun lg-struct-slot (getter setter &optional slot-id)
  "Build a struct-slot optic from GETTER and SETTER.

Use generated cl-defstruct accessors in GETTER/SETTER.  When SLOT-ID is
non-nil, return an indexed lens whose index is SLOT-ID."
  (if slot-id
      (lg-ilens getter setter (lambda (_source) slot-id))
    (lg-lens getter setter)))

(defun lg-tree-leaves (&optional pred)
  "Traversal over tree leaves.

Leaves are non-cons values.  When PRED is non-nil, only matching leaves are
focused."
  (lg-wander
   (lambda (step source app)
     (let ((pure (lg-applicative-pure app))
           (map2 (lg-applicative-map2 app)))
       (cl-labels ((walk (node)
                     (if (consp node)
                         (funcall map2 #'cons (walk (car node)) (walk (cdr node)))
                       (if (or (null pred) (funcall pred node))
                           (funcall step node)
                         (funcall pure node)))))
         (walk source))))))

(defun lg-itree-leaves (&optional pred)
  "Indexed traversal over tree leaves.

Indices are paths as lists of symbols `car' and `cdr' from root to leaf."
  (lg-iwander
   (lambda (step source app)
     (let ((pure (lg-applicative-pure app))
           (map2 (lg-applicative-map2 app)))
       (cl-labels ((walk (node path)
                     (if (consp node)
                         (funcall map2
                                  #'cons
                                  (walk (car node) (append path '(car)))
                                  (walk (cdr node) (append path '(cdr))))
                       (if (or (null pred) (funcall pred node))
                           (funcall step (cons path node))
                         (funcall pure node)))))
         (walk source nil))))))

(defconst lg-car
  (lg-lens #'car (lambda (new source) (cons new (cdr source))))
  "Lens focusing the `car' of a cons cell.")

(defconst lg-icar
  (lg-ilens #'car (lambda (new source) (cons new (cdr source)))
            (lambda (_source) 'car))
  "Indexed lens focusing the `car' of a cons cell.")

(defconst lg-cdr
  (lg-lens #'cdr (lambda (new source) (cons (car source) new)))
  "Lens focusing the `cdr' of a cons cell.")

(defconst lg-icdr
  (lg-ilens #'cdr (lambda (new source) (cons (car source) new))
            (lambda (_source) 'cdr))
  "Indexed lens focusing the `cdr' of a cons cell.")

(defun lg-gethash (key &optional test)
  "Lens focusing KEY in a hash table.

When TEST is non-nil, newly copied tables use that equality test."
  (lg-lens
   (lambda (source)
     (gethash key source))
   (lambda (new source)
     (let ((table (copy-hash-table source)))
       (when test
         (let ((rehydrated (make-hash-table :test test)))
           (maphash (lambda (k v) (puthash k v rehydrated)) table)
           (setq table rehydrated)))
       (puthash key new table)
       table))))

(defun lg-igethash (key &optional test)
  "Indexed lens focusing KEY in a hash table.

The index for the focus is KEY.  When TEST is non-nil, newly copied tables use
that equality test."
  (lg-ilens
   (lambda (source)
     (gethash key source))
   (lambda (new source)
     (let ((table (copy-hash-table source)))
       (when test
         (let ((rehydrated (make-hash-table :test test)))
           (maphash (lambda (k v) (puthash k v rehydrated)) table)
           (setq table rehydrated)))
       (puthash key new table)
       table))
   (lambda (_source) key)))

(defun lg-plist (prop)
  "Lens focusing PROP in a plist."
  (lg-lens
   (lambda (source)
     (plist-get source prop))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (plist-put copy prop new)
       copy))))

(defun lg-iplist (prop)
  "Indexed lens focusing PROP in a plist.

The index for the focus is PROP."
  (lg-ilens
   (lambda (source)
     (plist-get source prop))
   (lambda (new source)
     (let ((copy (copy-sequence source)))
       (plist-put copy prop new)
       copy))
   (lambda (_source) prop)))

(defun lg-each-list ()
  "Traversal focusing each element in a list."
  (lg-wander #'lg--list-traverse))

(defun lg-ieach-list ()
  "Indexed traversal focusing each element in a list.

Indices are zero-based integer positions."
  (lg-iwander #'lg--ilist-traverse))

(defun lg-each-vector ()
  "Traversal focusing each element in a vector."
  (lg-compose
   (lg-iso (lambda (vec) (append vec nil))
           (lambda (lst) (vconcat lst)))
   (lg-each-list)))

(defun lg-ieach-vector ()
  "Indexed traversal focusing each element in a vector.

Indices are zero-based integer positions."
  (lg-iwander
   (lambda (step source app)
     (let ((pure (lg-applicative-pure app))
           (map2 (lg-applicative-map2 app))
           (acc nil)
           (i 0)
           (len (length source)))
       (setq acc (funcall pure nil))
       (while (< i len)
         (let ((idx i))
           (setq acc
                 (funcall map2
                          (lambda (xs idx+value)
                            (append xs (list (cdr idx+value))))
                          acc
                          (funcall step (cons idx (aref source i))))))
         (setq i (1+ i)))
       (lg--app-map app #'vconcat acc)))))

(defun lg-just ()
  "Prism that matches non-nil values and reviews to themselves."
  (lg-prism
   (lambda (source)
     (when source
       (cons t source)))
   #'identity))

(defun lg-pred (predicate)
  "Prism that matches values satisfying PREDICATE."
  (lg-prism
   (lambda (source)
     (when (funcall predicate source)
       (cons t source)))
   #'identity))

(defun lg-regex (regexp &optional group)
  "Traversal over REGEXP matches in strings.

GROUP defaults to 0 (full match).  When GROUP is non-zero, only that capture
group is focused and rewritten."
  (lg-wander (lg--regex-traverse-builder regexp (or group 0))))

(defun lg-iregex (regexp &optional group)
  "Indexed traversal over REGEXP matches in strings.

GROUP defaults to 0 (full match).  When GROUP is non-zero, only that capture
group is focused and rewritten.  Indices are zero-based match ordinals."
  (lg-iwander (lg--iregex-traverse-builder regexp (or group 0))))

(lg--register-default-kinds)

(provide 'looking-glass)

;;; looking-glass.el ends here
