#!/bin/bash
set -euo pipefail

./cmake-build.sh
./build/tests
