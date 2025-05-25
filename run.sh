#!/bin/bash
set -e

./cmake-build.sh

./build/neutronium "$@"

set +e  # disable immediate exit on failure
./neutro/out
echo "Exit code: $?"
