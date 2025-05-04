# Neutronium

> ⚠️ **Disclaimer:** The Neutronium compiler works on **Linux only**, and the following instructions are tailored for Linux environments.

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

This command builds the Neutronium compiler and executes it on the specified source file.

To execute the generated machine code, run:

```bash
./neutro/out
```

> Note: you can write some scripts in the scripts/ directory, which is ignored by source control.
