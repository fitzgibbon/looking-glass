# API reference

This is a grouped reference of public APIs in `looking-glass`.

## By optic type

### Isomorphism

- Constructor: `lg-iso`
- Related review support: `lg-review`, `lg-unto`, `lg-reviews`
- Example built from convert package: `lg-list-vector-iso`

### Lens

- Constructor: `lg-lens`
- Common lenses: `lg-car`, `lg-cdr`, `lg-nth`, `lg-at`, `lg-unmaybe`

### Prism

- Constructor: `lg-prism`
- Common prisms: `lg-non-nil`, `lg-just-o`, `lg-left-o`, `lg-right-o`
- Convert prisms: `lg-number-string-prism`, `lg-char-string-prism`, `lg-symbol-string-prism`, `lg-alist-plist-prism`

### Traversal

- Constructor: `lg-traversal`
- Common traversals: `lg-list`, `lg-vector`, `lg-string`, `lg-filtered`
- Affine traversal constructor: `lg-affine`
- Keyed affine traversal: `lg-ix`, `lg-plist-key`, `lg-alist-key`, `lg-hash-key`

### Indexed traversal

- Constructor: `lg-indexed`
- Composition: `lg-compose-indexed`, `lg-compose-indexed2`
- Reindexing: `lg-ireindexed`
- Common indexed traversals: `lg-indexed-list`, `lg-ifiltered`, `lg-indices`, `lg-indexed-list-filtered`, `lg-indexed-list-indices`

### Getter/Fold-like helpers

- Getter constructor: `lg-getter` (alias: `lg-to`)
- Fold collection: `lg-to-list-of`, `lg-ito-list-of`
- Fold queries: `lg-first-of`, `lg-last-of`, `lg-find-of`, `lg-any-of`, `lg-none-of`, `lg-all-of`, `lg-count-of`
- Indexed fold queries: `lg-ifirst-of`, `lg-ilast-of`, `lg-ifind-of`, `lg-iany-of`, `lg-inone-of`, `lg-iall-of`, `lg-icount-of`
- Fold reducers: `lg-foldl-of`, `lg-ifoldl-of`

## By operation

### Compose

- `lg-compose`, `lg-compose2`, `lg-optic`

### Read

- `lg-view`
- `lg-view-non-nil`
- `lg-preview`
- `lg-preview-maybe` (alias)
- `lg-preview-or`
- `lg-ipreview`
- `lg-ipreview-maybe` (alias)
- `lg-has`

### Update

- `lg-over`
- `lg-set`
- `lg-iover`
- `lg-imap-of`

### Review/build

- `lg-review`
- `lg-unto`
- `lg-reviews`

## By data structure

### Pairs/cons

- `lg-car`, `lg-cdr`

### Lists

- `lg-list`, `lg-indexed-list`, `lg-nth`

### Vectors

- `lg-vector`

### Strings

- `lg-string`

### Keyed structures

- Generic keyed: `lg-ix`, `lg-ix-maybe`, `lg-at`, `lg-at-maybe` (alias)
- Plist helpers: `lg-plist-key`
- Alist helpers: `lg-alist-key`
- Hash helpers: `lg-hash-key`, `lg-hash-key-at`, `lg-hash-key-at-maybe`

### Maybe/Either-style values

- Tagged maybe values: `lg-just`, `lg-nothing`, `lg-just-p`, `lg-nothing-p`, `lg-maybe-value`
- Tagged either values: `lg-left`, `lg-right`, `lg-left-p`, `lg-right-p`, `lg-either-p`, `lg-either-value`
- Optics: `lg-non-nil`, `lg-just-o`, `lg-left-o`, `lg-right-o`

## Extension packages

### Conversion (`looking-glass-convert`)

- `lg-list-vector-iso`
- `lg-number-string-prism`
- `lg-char-string-prism`
- `lg-symbol-string-prism`
- `lg-alist-plist-prism`

### JSON (`looking-glass-json`)

- Key/object: `lg-json-object-key`, `lg-json-object-key-at`
- Arrays: `lg-json-array-index`
- Object traversals: `lg-json-values`, `lg-json-members`
- Scalars/prisms: `lg-json-string`, `lg-json-number`, `lg-json-bool`, `lg-json-null-prism`
- Text parse/review: `lg-json-text-prism`, `lg-json-parse-string`, `lg-json-render-string`

### YAML (`looking-glass-yaml`)

- Mapping: `lg-yaml-key`, `lg-yaml-key-at`
- Sequences: `lg-yaml-index`
- Scalars/prisms: `lg-yaml-string`, `lg-yaml-number`, `lg-yaml-bool`, `lg-yaml-null-prism`

### TOML (`looking-glass-toml`)

- Table keys: `lg-toml-key`, `lg-toml-key-at`
- Arrays: `lg-toml-array-index`
- Scalars/prisms: `lg-toml-string`, `lg-toml-number`, `lg-toml-bool`
- Text parse/review: `lg-toml-text-prism`

### Buffer (`looking-glass-buffer`)

- Buffer lenses: `lg-buffer-point`, `lg-buffer-mark`, `lg-buffer-string`
- Region and slices: `lg-buffer-region-string`, `lg-buffer-substring`
