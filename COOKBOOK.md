# looking-glass cookbook

Practical examples for common Emacs Lisp data transformations.

## Require packages

```elisp
(require 'looking-glass)
(require 'looking-glass-convert)
```

## Increment every number in a list

```elisp
(lg-over lg-list (lambda (x) (+ x 1)) '(1 2 3))
;; => (2 3 4)
```

## Update only one list position

```elisp
(lg-over (lg-nth 2) (lambda (x) (* x 10)) '(4 5 6 7))
;; => (4 5 60 7)
```

## Update nested cons cells without manual destructuring

```elisp
(setq nested '((1 . 2) . 3))
(setq inner-cdr (lg-compose lg-cdr lg-car))

(lg-view inner-cdr nested)
;; => 2

(lg-set inner-cdr 7 nested)
;; => ((1 . 7) . 3)
```

## Edit keyed data when key exists (`ix`)

```elisp
(setq person '(:name "Ada" :age 10))

(lg-set (lg-ix :age) 11 person)
;; => (:name "Ada" :age 11)

(lg-set (lg-ix :city) "London" person)
;; => (:name "Ada" :age 10)
```

## Insert or remove keyed data (`at`)

```elisp
(setq person '(:name "Ada" :age 10))

(lg-set (lg-at :city) (lg-just "London") person)
;; => (:name "Ada" :age 10 :city "London")

(lg-set (lg-at :age) lg-nothing person)
;; => (:name "Ada")

(lg-view (lg-at :city) person)
;; => lg-nothing

(lg-set (lg-at :nickname) (lg-just nil) person)
;; => (:name "Ada" :age 10 :nickname nil)

(lg-view (lg-compose lg-unmaybe (lg-at :city)) person)
;; => nil
```

## Keep missing and present-nil distinct

```elisp
(setq with-nil '(:name nil))

(lg-preview (lg-ix :name) with-nil)
;; => (lg-just)

(lg-preview (lg-ix :missing) with-nil)
;; => lg-nothing
```

## Filter updates by value

```elisp
(setq even-only (lg-filtered (lambda (x) (and (numberp x) (zerop (% x 2))))))

(lg-over even-only (lambda (x) (* x 10)) 2)
;; => 20

(lg-over even-only (lambda (x) (* x 10)) 3)
;; => 3
```

## Filter updates by index

```elisp
(setq even-indexes (lg-indexed-list-indices (lambda (i) (zerop (% i 2)))))

(lg-iover even-indexes (lambda (_i x) (+ x 100)) '(10 11 12 13))
;; => (110 11 112 13)
```

## Aggregate data with folds

```elisp
(setq xs '(1 2 3 4))

(lg-count-of lg-list xs)
;; => 4

(lg-any-of lg-list (lambda (x) (> x 3)) xs)
;; => t

(lg-foldl-of lg-list #'+ 0 xs)
;; => 10
```

## Construct values with review optics

```elisp
(setq wrap (lg-unto (lambda (x) (list :wrapped x))))

(lg-review wrap 9)
;; => (:wrapped 9)

(lg-reviews wrap #'car 9)
;; => :wrapped
```

## Work with Either-style prisms

```elisp
(lg-over lg-left-o (lambda (x) (+ x 1)) (lg-left 2))
;; => (left . 3)

(lg-over lg-left-o (lambda (x) (+ x 1)) (lg-right 2))
;; => (right . 2)
```

## Convert between string and number with prisms

```elisp
(lg-preview lg-number-string-prism "42")
;; => (lg-just . 42)

(lg-preview lg-number-string-prism "42x")
;; => lg-nothing

(lg-review lg-number-string-prism -3.5)
;; => "-3.5"
```
