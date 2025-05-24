#!/bin/bash
./cmake-build.sh
./build/neutronium "$@"
./neutro/out
echo $?