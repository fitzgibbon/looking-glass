#!/usr/bin/env bash
set -euo pipefail

INFO_DIR="${1:-build/manuals/info}"
TARGET_BRANCH="${TARGET_BRANCH:-melpa-release}"

if [[ ! -d "${INFO_DIR}" ]]; then
  echo "Missing info directory: ${INFO_DIR}" >&2
  exit 1
fi

echo "DRY RUN: MELPA manual publish"
echo "Would publish generated manuals to branch: ${TARGET_BRANCH}"
echo ""
echo "Would copy these files to repository root (required for package.el Info autodiscovery):"
for file in "${INFO_DIR}"/*.info "${INFO_DIR}/dir"; do
  [[ -e "${file}" ]] || continue
  echo "  - $(basename "${file}")"
done
echo ""
echo "Would execute commands:"
echo "  git fetch origin"
echo "  git checkout -B ${TARGET_BRANCH} origin/main"
echo "  cp ${INFO_DIR}/*.info ."
echo "  cp ${INFO_DIR}/dir ."
echo "  git add -- *.info dir"
echo "  git commit -m 'Publish sources with generated Info manuals for MELPA packaging'"
echo "  git push --force-with-lease origin ${TARGET_BRANCH}"
