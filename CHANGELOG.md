# Changelog

All notable changes to this project are documented in this file.

## [0.1.0] - 2026-03-02

Initial public release.

- Added profunctor optic core with `lg-iso`, `lg-lens`, `lg-prism`, `lg-traversal`, and `lg-affine`.
- Added interpreters and utilities for viewing, previewing, collecting, and updating focuses.
- Added indexed optics and indexed fold/update operations.
- Added keyed optics across plist, alist, and hash-table sources with missing-vs-present-`nil` support.
- Added conversion optics (`lg-list->vector`, `lg-number-string`, etc.) in core.
- Added README quickstart, cookbook, API reference, and naming/semantics comparison docs.
- Added broader randomized/property-style tests and benchmark harness for hot-path profiling.
