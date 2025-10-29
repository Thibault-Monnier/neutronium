<img src="assets/Logo.png" width="324"/>

[![Build all targets](https://github.com/Thibault-Monnier/neutronium/actions/workflows/build.yml/badge.svg)](https://github.com/Thibault-Monnier/neutronium/actions/workflows/build.yml) [![Run Tests](https://github.com/Thibault-Monnier/neutronium/actions/workflows/test.yml/badge.svg)](https://github.com/Thibault-Monnier/neutronium/actions/workflows/test.yml) [![codecov](https://codecov.io/gh/Thibault-Monnier/neutronium/graph/badge.svg?token=VAEY97VTE3)](https://codecov.io/gh/Thibault-Monnier/neutronium)

## Neutronium

Neutronium is a lightweight C-like programming language built as an educational project.

> **Disclaimer:** The Neutronium compiler works on **Linux only**, and the following instructions are tailored for
> Linux (Ubuntu) environments.

<p>
  <br>
  <a href="https://github.com/Thibault-Monnier/neutronium/releases/latest">
  📦 <strong>Latest Release</strong>
  </a>
</p>

## Setup

Clone this repository and set it up by running:

```bash
git clone https://github.com/Thibault-Monnier/neutronium.git
cd neutronium
chmod +x cmake-build.sh
chmod +x compile.sh
chmod +x run.sh
chmod +x test.sh
```

To install the required dependencies, run:

```bash
sudo apt update
sudo apt install build-essential cmake ninja-build llvm
```

Make sure your `cmake` version is at least 3.28.

## Compile and Run

You can use the following scripts to compile and run the Neutronium compiler:

```bash
./configure_cmake.sh # Reconfigure cmake build files
./compile.sh <path-to-source-file> [OPTIONS] # Build the compiler, then compile a Neutronium source file
./run.sh <path-to-source-file> [OPTIONS] # Build the compiler, then compile and run a Neutronium source file
```

As of October 2025, the compiler always writes the generated executable to `neutro/out`.

> Tip: you can write your scripts in the scripts/ directory, which is ignored by version control.

## Run Tests

To build and run the test suite, run:

```bash
./test.sh
```

## Contributing

Contributions are welcome! If you find an issue, please report it to
the [Neutronium bug tracker](https://github.com/Thibault-Monnier/neutronium/issues).

## Language

Refer to the [language documentation](docs/language.md) for syntax.

