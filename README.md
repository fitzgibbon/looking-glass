# looking-glass

`looking-glass` provides profunctor optics for Emacs Lisp.

It includes core optics (`iso`, `lens`, `prism`, `traversal`), indexed variants,
keyed optics (`ix`, `at`), and convenience operations for viewing, updating, and folding.

## Quickstart

```elisp
(require 'looking-glass)
(require 'looking-glass-convert)
(require 'looking-glass-json)
(require 'looking-glass-yaml)
(require 'looking-glass-toml)
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

### 8) Conversion optics (`looking-glass-convert`)

```elisp
(lg-preview lg-number-string-prism "  -3.5 ")
;; => (lg-just . -3.5)

(lg-review lg-number-string-prism 15)
;; => "15"
```

## Main API groups

- Core constructors: `lg-iso`, `lg-lens`, `lg-prism`, `lg-traversal`, `lg-affine`
- Composition: `lg-compose`, `lg-compose-indexed`, `lg-optic`
- Sum helpers: `lg-just`/`lg-nothing` (Maybe), `lg-left`/`lg-right` (Either)
- Keyed optics: `lg-ix`, `lg-at`, `lg-plist-key`, `lg-alist-key`, `lg-hash-key`
- Sum-type optics: `lg-just-o`, `lg-left-o`, `lg-right-o`, `lg-non-nil`
- Maybe adapters: `lg-unmaybe`
- Viewing and updates: `lg-view`, `lg-preview`, `lg-over`, `lg-set`
- Folds: `lg-to-list-of`, `lg-first-of`, `lg-last-of`, `lg-find-of`, `lg-any-of`, `lg-all-of`
- Indexed folds and updates: `lg-ito-list-of`, `lg-ifirst-of`, `lg-ilast-of`, `lg-ifind-of`, `lg-iover`
- Review helpers: `lg-review`, `lg-unto`, `lg-reviews`

### Extension packages

- `looking-glass-convert`: conversion optics (`lg-list-vector-iso`, `lg-number-string-prism`, ...)
- `looking-glass-json`: JSON object/array/scalar/text optics (`lg-json-object-key`, `lg-json-array-index`, `lg-json-text-prism`, ...)
- `looking-glass-yaml`: YAML mapping/sequence/scalar optics (`lg-yaml-key`, `lg-yaml-index`, ...)
- `looking-glass-toml`: TOML table/array/scalar optics (`lg-toml-key`, `lg-toml-array-index`, `lg-toml-text-prism`, ...)
- `looking-glass-buffer`: buffer-oriented optics (`lg-buffer-point`, `lg-buffer-string`, `lg-buffer-region-string`, ...)

## Documentation

- Cookbook: `COOKBOOK.md`
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
