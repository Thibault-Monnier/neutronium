"""
Generate a large Neutronium benchmark file by combining a main source file and random variants.

Usage:
    python3 generate_bench.py <source_directory> <num_variants> [output_filename]

This script will:
  - Read benchmark/tests/<source_directory>/main.nt
  - Append 20,000 randomly chosen variant_i.nt files (1 ≤ i ≤ num_variants)
  - Replace all instances of "$ID$" with the current iteration index
  - Write the result to benchmark/generated/<source_dir_name>/<output_filename> (default: bench_final.nt)
  - Produce deterministic but decorrelated variant ordering based on the directory name
"""

import hashlib
import random
import sys
from pathlib import Path


class Color:
    BLUE = "\033[94m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    RED = "\033[91m"
    BOLD = "\033[1m"
    RESET = "\033[0m"


def info(msg: str) -> None:
    print(f"{Color.BLUE}[INFO]{Color.RESET} {msg}")


def warn(msg: str) -> None:
    print(f"{Color.YELLOW}[WARN]{Color.RESET} {msg}")


def error(msg: str) -> None:
    print(f"{Color.RED}[ERROR]{Color.RESET} {msg}")


def success(msg: str) -> None:
    print(f"{Color.GREEN}[DONE]{Color.RESET} {msg}")


def main() -> None:
    if len(sys.argv) < 3:
        print(f"{Color.BOLD}Usage:{Color.RESET} python3 generate_bench.py "
              "<source_directory> <num_variants> [output_filename]")
        sys.exit(1)

    base_dir = (Path("tests") / sys.argv[1]).resolve()
    max_variant = int(sys.argv[2])
    output_filename = sys.argv[3] if len(sys.argv) > 3 else "bench_final.nt"

    main_file = base_dir / "main.nt"
    iterations = 20_000

    if not main_file.is_file():
        error(f"Main file not found: {main_file}")
        sys.exit(1)

    # Prepare output directory: generated/<source_dir_name>/
    output_dir = Path("generated") / base_dir.name
    output_dir.mkdir(parents=True, exist_ok=True)
    output_path = output_dir / output_filename

    # Seed based on directory name and number of variants
    seed_input = f"{base_dir.name}:{max_variant}".encode("utf-8")
    seed_value = int.from_bytes(hashlib.sha256(seed_input).digest()[:8], "little")
    rng = random.Random(seed_value)

    info(f"Building benchmark file: {Color.BOLD}{output_path}{Color.RESET}")
    info(f"Source directory:        {base_dir}")
    info(f"Variants:                1..{max_variant}")
    info(f"Iterations:              {iterations:,}")
    info(f"Seed (deterministic):    {seed_value}\n")

    # Read main file once
    with open(main_file, "r", encoding="utf-8") as f:
        main_content = f.read()

    with open(output_path, "w", encoding="utf-8") as out:
        out.write(main_content)
        out.write("\n\n")

        for i in range(1, iterations + 1):
            variant_index = rng.randint(1, max_variant)
            variant_file = base_dir / f"variant_{variant_index}.nt"

            if not variant_file.is_file():
                error(f"Missing variant file: {variant_file}")
                sys.exit(1)

            with open(variant_file, "r", encoding="utf-8") as vf:
                content = vf.read().replace("$ID$", f"_{i}")

            out.write(content)
            out.write("\n\n")

            if i % 1000 == 0:
                info(f"Appended {i:,}/{iterations:,} variants")

    # Count total lines
    with open(output_path, "r", encoding="utf-8") as final_file:
        total_lines = sum(1 for _ in final_file)

    success(f"Generated {output_path} ({Color.BOLD}{total_lines:,}{Color.RESET} lines total)")


# --------------------------------------------------------------------------- #
if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print(f"\n{Color.RED}Aborted by user.{Color.RESET}")
        sys.exit(1)
