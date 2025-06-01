set -e  # exit immediately if a command exits with a non-zero status

./cmake-build.sh

./build/neutronium "$@"

set +e  # disable immediate exit on failure