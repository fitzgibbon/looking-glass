#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."

emacs -Q --batch -L . -L test \
  -l test/looking-glass-test.el \
  -f ert-run-tests-batch-and-exit
