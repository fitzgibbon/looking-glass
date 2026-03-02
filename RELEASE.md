# Release Process

This project follows semantic versioning (`MAJOR.MINOR.PATCH`).

## Versioning policy

- `MAJOR`: breaking API changes or behavior changes in documented semantics.
- `MINOR`: backwards-compatible features and new optics/operations.
- `PATCH`: backwards-compatible bug fixes, perf improvements, and docs/tests-only fixes.

## Release checklist

1. Update version in package headers if needed:
   - `looking-glass.el`
   - `looking-glass-pkg.el`
2. Update `CHANGELOG.md` with a dated entry for the release.
3. Run tests:

   ```bash
   emacs -Q --batch -L . -L test -l test/looking-glass-test.el -f ert-run-tests-batch-and-exit
   ```

4. Run microbenchmarks and compare against baseline:

   ```bash
   emacs -Q --batch -L . -L bench -l bench/looking-glass-bench.el -f lg-bench-run
   ```

5. Commit release changes.
6. Tag release:

   ```bash
   git tag -a vX.Y.Z -m "looking-glass vX.Y.Z"
   ```

7. Push commit and tag:

   ```bash
   git push origin main
   git push origin vX.Y.Z
   ```

## v0.1.0 tagging command

After committing release artifacts for 0.1.0:

```bash
git tag -a v0.1.0 -m "looking-glass v0.1.0"
```
