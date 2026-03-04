#!/usr/bin/env bash
set -euo pipefail

OUT_DIR="${1:-build/manuals}"
TEXI_DIR="${OUT_DIR}/texi"
INFO_DIR="${OUT_DIR}/info"

mkdir -p "${TEXI_DIR}" "${INFO_DIR}"

emacs -Q --batch -L . -l scripts/export-manuals.el -- "${TEXI_DIR}"

for texi in "${TEXI_DIR}"/*.texi; do
  base_name="$(basename "${texi}" .texi)"
  makeinfo --no-split -o "${INFO_DIR}/${base_name}.info" "${texi}"
done

dir_file="${INFO_DIR}/dir"
: > "${dir_file}"

if command -v install-info >/dev/null 2>&1; then
  for info_file in "${INFO_DIR}"/*.info; do
    install-info --dir-file="${dir_file}" "${info_file}" >/dev/null 2>&1
  done
else
  echo "install-info not found; skipping dir menu generation"
fi

echo "Built manuals in ${OUT_DIR}"
echo "Texinfo files: ${TEXI_DIR}"
echo "Info files and dir: ${INFO_DIR}"
