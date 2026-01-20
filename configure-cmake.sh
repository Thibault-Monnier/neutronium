#!/bin/bash
set -euo pipefail

build_type=""
if [[ $# -ge 1 ]]; then
    case "$1" in
        Debug|Release|RelWithDebInfo)
            build_type="-DCMAKE_BUILD_TYPE=$1"
            shift
            ;;
    esac
fi

cmake -B build -G Ninja \
    ${build_type:+$build_type} \
    "$@" \
    .
