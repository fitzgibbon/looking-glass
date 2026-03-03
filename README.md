# looking-glass

`looking-glass` provides profunctor optics for Emacs Lisp.

It includes core optics (`iso`, `lens`, `prism`, `traversal`), indexed variants,
keyed optics (`ix`, `at`), and convenience operations for viewing, updating, and folding.

## Quickstart

```elisp
(require 'looking-glass)
(require 'looking-glass-buffer)
```

### 1) Read and update nested pair fields

```elisp
(setq pair-optic (lg-compose lg-cdr lg-car))

(lg-view pair-optic '((1 . 2) . 3))
;; => 2

(lg-set pair-optic 7 '((1 . 2) . 3))
;; => ((1 . 7) . 3)
```

### 1b) Compose with operator-style helpers

```elisp
;; Right-to-left composition (same as `lg-compose`)
(lg-view (lg<< lg-cdr lg-car) '((1 . 2) . 3))
;; => 2

;; Left-to-right composition (pipeline style)
(lg-view (lg>> lg-car lg-cdr) '((1 . 2) . 3))
;; => 2
```

### 2) Update all list items through a traversal

```elisp
(lg-over lg-list (lambda (x) (* x 10)) '(1 2 3))
;; => (10 20 30)
```

### 3) Preview and construct with a prism

```elisp
(setq non-nil lg-non-nil)

(lg-preview non-nil nil)
;; => lg-nothing

(lg-review non-nil 42)
;; => 42
```

### 4) Distinguish missing focus from present `nil`

```elisp
(setq user '(:name nil :age 10))

(lg-preview (lg-ix :name) user)
;; => (lg-just)

(lg-preview (lg-ix :missing) user)
;; => lg-nothing
```

### 5) Use `ix` for update-only-when-present

```elisp
(lg-set (lg-ix :age) 11 '(:name "Ada" :age 10))
;; => (:name "Ada" :age 11)

(lg-set (lg-ix :missing) 11 '(:name "Ada" :age 10))
;; => (:name "Ada" :age 10)
```

### 6) Use `at` with tagged maybe (or compose `lg-unmaybe`)

```elisp
(setq profile '(:name "Ada" :age 10))

(lg-view (lg-at :name) profile)
;; => (lg-just . "Ada")

(lg-set (lg-at :city) (lg-just "London") profile)
;; => (:name "Ada" :age 10 :city "London")

(lg-set (lg-at :age) lg-nothing profile)
;; => (:name "Ada")

(lg-view (lg-compose lg-unmaybe (lg-at :name)) profile)
;; => "Ada"
```

### 7) Indexed traversals expose index + value

```elisp
(lg-ito-list-of lg-indexed-list '(10 11 12))
;; => ((0 . 10) (1 . 11) (2 . 12))

(lg-iover lg-indexed-list (lambda (i x) (+ i x)) '(10 11 12))
;; => (10 12 14)
```

### 8) Conversion optics

```elisp
(lg-preview lg-number-string "  -3.5 ")
;; => (lg-just . -3.5)

(lg-review lg-number-string 15)
;; => "15"
```

### 9) Update one list position

```elisp
(lg-over (lg-nth 2) (lambda (x) (* x 10)) '(4 5 6 7))
;; => (4 5 60 7)
```

### 10) Filter by value or index

```elisp
(setq even-only (lg-filtered (lambda (x) (and (numberp x) (zerop (% x 2))))))
(lg-over even-only (lambda (x) (* x 10)) 3)
;; => 3

(setq even-indexes (lg-indexed-list-indices (lambda (i) (zerop (% i 2)))))
(lg-iover even-indexes (lambda (_i x) (+ x 100)) '(10 11 12 13))
;; => (110 11 112 13)
```

### 11) Parse and edit JSON text with composed optics

```elisp
(lg-over
 (lg-compose (lg-ix "name") lg-json-parse)
 #'upcase
 "{\"name\":\"Ada\",\"age\":10}")
;; => "{\"name\":\"ADA\",\"age\":10}"

(lg-over
 (lg-compose (lg-ix "tags") lg-json-parse)
 (lambda (tags)
   (let ((copy (copy-sequence tags)))
     (aset copy 1 (upcase (aref copy 1)))
     copy))
 "{\"tags\":[\"elisp\",\"optics\"]}")
;; => "{\"tags\":[\"elisp\",\"OPTICS\"]}"
```

### 12) Traverse JSON object values and members

```elisp
(setq doc (lg-view lg-json-parse "{\"name\":\"Ada\",\"age\":10}"))

(gethash "name" doc)
;; => "Ada"

(gethash "age" doc)
;; => 10

(gethash "active" (lg-view lg-json-parse "{\"active\":true}"))
;; => lg-true
```

## Main API groups

- Core constructors: `lg-iso`, `lg-lens`, `lg-prism`, `lg-traversal`, `lg-affine`
- Composition: `lg-compose`, `lg<<`, `lg>>`, `lg-compose-indexed`, `lg-optic`
- Sum helpers: `lg-just`/`lg-nothing` (Maybe), `lg-left`/`lg-right` (Either)
- Keyed optics: `lg-ix`, `lg-at`, `lg-plist-key`, `lg-alist-key`, `lg-hash-key`
- Sum-type optics: `lg-just-o`, `lg-left-o`, `lg-right-o`, `lg-non-nil`
- Maybe adapters: `lg-unmaybe`
- Viewing and updates: `lg-view`, `lg-preview`, `lg-over`, `lg-set`
- Folds: `lg-to-list-of`, `lg-first-of`, `lg-last-of`, `lg-find-of`, `lg-any-of`, `lg-all-of`
- Indexed folds and updates: `lg-ito-list-of`, `lg-ifirst-of`, `lg-ilast-of`, `lg-ifind-of`, `lg-iover`
- Review helpers: `lg-review`, `lg-unto`, `lg-reviews`

### Extension packages

- `looking-glass-buffer`: buffer-oriented optics (`lg-buffer-point`, `lg-buffer-string`, `lg-buffer-region-string`, ...)

## Documentation

- API reference by optic type and data structure: `API-REFERENCE.md`
- Naming/semantics comparison vs Haskell `lens` and PureScript `profunctor-lenses`: `COMPARISON.md`
- Changelog: `CHANGELOG.md`
- Release process and versioning policy: `RELEASE.md`

## Benchmarks

Run the microbenchmark suite:

```bash
emacs -Q --batch -L . -L bench -l bench/looking-glass-bench.el -f lg-bench-run
```

## Notes on semantics

- `lg-preview` returns tagged maybe: `lg-nothing` or `(lg-just . VALUE)`.
- `lg-at` focuses tagged maybe; setting `lg-nothing` removes and setting `(lg-just . VALUE)` inserts/updates.
- Compose with `lg-unmaybe` when you intentionally want nil-ambiguous shorthand behavior.
