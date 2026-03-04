#!/usr/bin/env bash
set -euo pipefail

INFO_DIR="${1:-build/manuals/info}"
TAG_NAME="${TAG_NAME:-v0.0.0-dry-run}"
ARCHIVE="build/manuals/looking-glass-info-${TAG_NAME}.tar.gz"

if [[ ! -d "${INFO_DIR}" ]]; then
  echo "Missing info directory: ${INFO_DIR}" >&2
  exit 1
fi

echo "DRY RUN: GitHub release publish"
echo "Would package Info manuals from: ${INFO_DIR}"
echo "Would create archive: ${ARCHIVE}"
echo ""
echo "Would execute commands:"
echo "  tar -czf ${ARCHIVE} -C ${INFO_DIR} ."
echo "  gh release create ${TAG_NAME} ${ARCHIVE} --title 'Release ${TAG_NAME}' --notes-file CHANGELOG.md"
