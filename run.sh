#!/bin/bash

set -e
./compile.sh "$@"
set +e

./neutro/out

echo "Exit code: $?"
