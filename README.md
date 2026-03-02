# looking-glass

`looking-glass` provides profunctor optics for Emacs Lisp.

It includes core optics (`iso`, `lens`, `prism`, `traversal`), indexed variants,
keyed optics (`ix`, `at`), and convenience operations for viewing, updating, and folding.

## Quickstart

```elisp
(require 'looking-glass)

;; Compose lenses
(setq pair-optic (lg-compose (lg-cdr-lens) (lg-car-lens)))
(lg-view pair-optic '((1 . 2) . 3))
;; => 2

;; Update through a traversal
(lg-over (lg-list-traversal) (lambda (x) (* x 10)) '(1 2 3))
;; => (10 20 30)

;; Prism preview/review
(setq non-nil (lg-non-nil))
(lg-preview non-nil nil)
;; => nil
(lg-review non-nil 42)
;; => 42

;; Disambiguated preview (missing vs present nil)
(lg-preview-result (lg-ix :name) '(:name nil :age 10))
;; => (t . nil)
(lg-preview-result (lg-ix :missing) '(:name nil :age 10))
;; => (nil . nil)

;; Keyed optics over plist/alist/hash-table
(lg-set (lg-ix :age) 11 '(:name "Ada" :age 10))
;; => (:name "Ada" :age 11)

;; `at` focuses explicit presence/value state
(lg-view (lg-at :age) '(:age nil))
;; => (t . nil)
(lg-set (lg-at :age) '(nil . ignored) '(:name "Ada" :age 10))
;; => (:name "Ada")

;; Indexed traversal
(lg-ito-list-of (lg-indexed-list-traversal) '(10 11 12))
;; => ((0 . 10) (1 . 11) (2 . 12))
```

## Main API groups

- Core constructors: `lg-iso`, `lg-lens`, `lg-prism`, `lg-traversal`, `lg-affine-traversal`
- Keyed optics: `lg-ix`, `lg-at`, `lg-plist-key-traversal`, `lg-alist-key-traversal`, `lg-hash-key-traversal`
- Viewing and updates: `lg-view`, `lg-preview`, `lg-preview-result`, `lg-over`, `lg-set`
- Folds: `lg-to-list-of`, `lg-first-of`, `lg-last-of`, `lg-find-of`, `lg-any-of`, `lg-all-of`
- Indexed folds and updates: `lg-ito-list-of`, `lg-ifirst-of`, `lg-ilast-of`, `lg-ifind-of`, `lg-iover`
- Review helpers: `lg-review`, `lg-unto`, `lg-reviews`

## Notes on semantics

- `lg-preview` returns `nil` for both missing focus and present `nil` focus.
- `lg-preview-result` returns `(FOUND . VALUE)` to disambiguate these cases.
- `lg-at` uses the same `(FOUND . VALUE)` shape as its focus, so insertion/removal is explicit.
