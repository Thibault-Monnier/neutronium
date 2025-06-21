#!/bin/bash
set -euo pipefail

./compile.sh "$@"

set +e

./neutro/out

echo "Exit code: $?"
