# comparison with Haskell and PureScript optics libraries

`looking-glass` is inspired by Haskell `lens` and PureScript `profunctor-lenses`,
but adapted to Emacs Lisp conventions.

## Naming correspondence

| Concept | Haskell `lens` | PureScript `profunctor-lenses` | `looking-glass` |
|---|---|---|---|
| lens constructor | `lens` | `lens` | `lg-lens` |
| prism constructor | `prism` / `prism'` | `prism` / `prism'` | `lg-prism` |
| traversal constructor | `traversal`-style combinators | `wander`-based traversal constructors | `lg-traversal` |
| affine traversal | commonly encoded via traversal/prism/indexed combinators | `Data.Lens.AffineTraversal` | `lg-affine` |
| view | `view` / `(^.)` | `view` | `lg-view` |
| preview | `preview` / `(^?)` | `preview` | `lg-preview` |
| set | `set` / `(.~)` | `set` | `lg-set` |
| over | `over` / `(%~)` | `over` | `lg-over` |
| list foci | `toListOf` / `(^..)` | `toListOf` | `lg-to-list-of` |
| has | `has` | `is`/`has` style helpers | `lg-has` |
| review | `review` / `(#)` | `review` | `lg-review` |
| `ix` | `ix` | `ix` | `lg-ix` |
| `at` | `at` | `at` | `lg-at` |

## Semantics differences

### Missing vs present `nil`

- Haskell/PureScript usually use `Maybe` for optional focus values.
- In Emacs Lisp, `nil` is both false and the empty list, and can be a real payload.
- `looking-glass` defaults to tagged maybe distinction for preview-like reads:
  - missing focus: `lg-nothing`
  - present focus (possibly `nil`): `(lg-just . VALUE)`
- Compose with `lg-unmaybe` only when you intentionally want nil-ambiguous behavior.

### Runtime checks vs static guarantees

- Haskell and PureScript enforce many optic constraints in the type system.
- Emacs Lisp cannot encode these constraints statically in the same way.
- `looking-glass` validates shape/usage at runtime and signals errors when needed.

### Data model adaptation

- `lens` and `profunctor-lenses` target typed ADTs, records, and standard typeclass instances.
- `looking-glass` targets idiomatic Elisp data:
  - cons cells and lists
  - plists and alists
  - hash tables
  - vectors and strings

### `ix` and `at`

- Conceptually aligned with Haskell/PureScript:
  - `ix` focuses an existing key/index (no insertion)
  - `at` focuses presence/value and supports insertion/removal
- In `looking-glass`, keyed containers are dynamically dispatched (plist/alist/hash table) rather than selected via static typeclass resolution.

## Design intent

- Keep the profunctor optic core and composition model familiar.
- Favor explicit, discoverable `lg-` APIs over operator-heavy style.
- Preserve practical parity for day-to-day transformations on Emacs Lisp structures.

## Current boundary policy (`ix` vs positional optics)

- Haskell `lens` and PureScript `profunctor-lenses` expose a polymorphic `ix` across map-like and sequence-like containers via typeclasses.
- `looking-glass` currently keeps this boundary explicit:
  - keyed/map-like containers use `lg-ix` and `lg-at`
  - sequence positions use positional optics such as `lg-nth`
- This is intentional in a dynamic language to avoid ambiguous runtime dispatch for list-shaped values (for example, plain lists vs alists).
- Future unification is possible, but should only happen with a documented dispatch policy and compatibility notes.
