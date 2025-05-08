# Neutronium

[![Build all targets](https://github.com/Thibault-Monnier/neutronium/actions/workflows/build.yml/badge.svg)](https://github.com/Thibault-Monnier/neutronium/actions/workflows/build.yml)

Neutronium is a lightweight C-like programming language built as an educational project.

> ⚠️ **Disclaimer:** The Neutronium compiler works on **Linux only**, and the following instructions are tailored for
> Linux environments.

<p>
  <br>
  <a href="https://github.com/Thibault-Monnier/neutronium/releases/latest">
    <mark>📦 <strong>Latest Release</strong></mark>
  </a>
</p>

## Setup

Clone this repository and set it up by running:

```bash
git clone https://github.com/Thibault-Monnier/neutronium.git
cd neutronium
chmod +x build.sh
```

## Compile and Run

From the root directory, compile and run Neutronium with:

```bash
./build.sh && ./build/neutronium <path-to-source-file>
```

This command builds the compiler and executes it on the provided source file.

To execute the generated machine code, run:

```bash
./neutro/out
```

> Note: you can write your scripts in the scripts/ directory, which is ignored by source control.

## Language

See the [full language documentation](docs/language.md) for details on the Neutronium language with syntax, examples, grammar and semantics.

