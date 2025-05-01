#!/bin/bash
cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Debug "$@" .
cmake --build build