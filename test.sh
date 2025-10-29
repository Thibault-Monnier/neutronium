#!/bin/bash
set -euo pipefail

cmake --build build

./build/tests
