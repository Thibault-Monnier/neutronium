## Intro

This directory contains a framework for creating large Neutronium benchmark files to test the performance of the
compiler.

## Creating New Benchmarks

You will need to create a new directory under `tests/` that will contain the source files for your benchmark.
Each benchmark directory should include:

- a `main.nt` file,
- any number of `variant_*.nt` files where indices are contiguous starting from 1.

The `main.nt` file is the base of the benchmark. It will be copied a single time at the top of the final built
benchmark.

The `variant_*.nt` files are variations that will be copied many times to increase the size of the benchmark. To prevent
name collisions, you can use the `$ID$` placeholder that will be replaced with a unique index during the build process.
The variant files will be copied N times in a random but deterministic order. In particular, a specific variant file may
not be included in the final build at all, while others may be included multiple times.

Having multiple variant files allows you to create more diverse benchmarks that better simulate real-world codebases. It
creates more variation in the code, which prevents the compiler from overfitting because of branch prediction or caching
effects.

## Building Benchmarks

To build a benchmark, you can use the `build_benchmark.py` script in this directory. It will create a final benchmark
file according to the protocol described above. The output file will be placed in
`generated/<source_dir>/<output_filename>`, which is ignored by version control.

You can then use the generated benchmark file to test the performance of the compiler.