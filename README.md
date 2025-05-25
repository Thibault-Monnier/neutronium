# Neutronium

[![Build all targets](https://github.com/Thibault-Monnier/neutronium/actions/workflows/build.yml/badge.svg)](https://github.com/Thibault-Monnier/neutronium/actions/workflows/build.yml) [![Run Tests](https://github.com/Thibault-Monnier/neutronium/actions/workflows/test.yml/badge.svg)](https://github.com/Thibault-Monnier/neutronium/actions/workflows/test.yml) [![codecov](https://codecov.io/gh/Thibault-Monnier/neutronium/graph/badge.svg?token=VAEY97VTE3)](https://codecov.io/gh/Thibault-Monnier/neutronium)

Neutronium is a lightweight C-like programming language built as an educational project.

> ⚠️ **Disclaimer:** The Neutronium compiler works on **Linux only**, and the following instructions are tailored for
> Linux (Ubuntu) environments.

<p>
  <br>
  <a href="https://github.com/Thibault-Monnier/neutronium/releases/latest">
    <mark>📦 <strong>Latest Release</strong></mark>
  </a>
</p>

## 🚀 Setup

Clone this repository and set it up by running:

```bash
git clone https://github.com/Thibault-Monnier/neutronium.git
cd neutronium
chmod +x cmake-build.sh
chmod +x run.sh
chmod +x test.sh
```

To install the required dependencies, run:

```bash
sudo apt update
sudo apt install build-essential cmake ninja-build nasm
````

## 🛠️ Compile and Run

Display the help message for the compiler by running:

```bash
./run.sh -h
```

To build the compiler and run it on a Neutronium source file, run:

```bash
./run.sh <path-to-source-file> [OPTIONS]
```

Alternatively, you can build the compiler and run it, then execute the machine code manually by executing:

```bash
./cmake-build.sh # Build the compiler
./build/neutronium <path-to-source-file> [OPTIONS] # Run the compiler on a source file
./neutro/out # Execute the generated machine code
echo $? # Print the exit code of the Neutronium program
```

> 💡 Tip: you can write your scripts in the scripts/ directory, which is ignored by version control.

## 🧪 Run Tests

To build and run the test suite, run:

```bash
./test.sh
```

## 🤝 Contributing

Contributions are welcome! Feel free to open an issue or to submit a pull request.

## 📚 Language

Refer to the [language documentation](docs/language.md) for syntax.

