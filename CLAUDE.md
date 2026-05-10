# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

UTS is a Scheme bytecode interpreter written in C, targeting R4RS compliance. It includes a compiler (written in Scheme) that compiles Scheme source to bytecode, and a runtime system with a REPL, debugger, and disassembler.

## Build Commands

```bash
# Configure (auto-detects Boehm GC location)
./configure

# Build release version (default)
make

# Build debug version (with assertions, much slower)
make DEBUG=1

# Clean and rebuild
make clean

# Install to /usr/local (or prefix set by configure)
make install
```

## Dependencies

- **Boehm GC**: Required. The `configure` script auto-detects it from: local `gc/` subdirectory, pkg-config, or common prefixes (/usr/local, /opt/homebrew, /usr)
- **GCC**: Required for `inline` functions and 64-bit `long long int` support
- **AWK**: Required to generate `instruc-cases.c` from `instrucs.c`
- A Scheme system to bootstrap from. Choices known to work are Chez Scheme, Guile, and (if you have a prebuilt binary) UTS itself.

## Running

```bash
# Start REPL
./uts

# Run a Scheme file
./uts program.scm arg1 arg2
```

## Bootstrap: compiling the compiler from Scheme to C

UTS is partly in Scheme, notably the reader and the bytecode compiler
themselves. This Scheme code needs to be precompiled into a C literal
array representing the initial heap (`init.c`). The Makefile takes
care of this.

Some C and Scheme source files get generated from two data files:
-`instrucs` lists the bytecode instructions (byteops)
- `prims` lists the primitive procedures

The rest of the Scheme source to be precompiled into `init.c` is in
`abcs/*.scm`. The boot Scheme (Chez by default) loads
`abcs/compiler.scm` and calls this code to compile the source files
into `init.c`; so the text of `compiler.scm` is processed twice:
loaded into the boot Scheme, then read and compiled by itself under
the boot Scheme. So the Scheme dialect it's coded in must be supported
by both. (TODO maybe worth elaborating)

## Architecture

### Core Components

- **uts.c**: Main C interpreter with bytecode VM, object representation, reader/writer, and garbage collection interface
- **byteops.c**: a case for each byteop, #included in `uts.c`.
- **config.h**: Platform-specific configuration (word sizes, stack limits)
- **abcs/primitives.scm**: the R4RS primitive procedures that aren't in C.
- **abcs/read.scm**: `read` is big enough to get its own file.
- **abcs/compiler.scm**: compiles s-expressions to bytecode.
- **abcs/dev-env.scm**: the REPL, debugger, etc.

### Object Representation

Tagged pointers with low 2 bits encoding type:
- `00`: Boxed object pointer (flonum, port, string, pair, closure, symbol, vector)
- `01`: Fixnum (62-bit signed integer)
- `11`: Special values (chars, booleans, eof, nil, unbound)

Boxed objects have a header with size and 3-bit tag.

### Bytecode VM

Stack machine with 22 instructions designed for Scheme
only.

Environments are linked vectors on the heap (one vector per scope
level; link to parent in 0th slot). The variable names are separate,
in debug info.

The stack is organized in frames; at the top of a frame not currently
live are stored: return PC, code vector, lexical environment, previous
frame pointer.

### Compiler (in abcs/compiler.scm)

The `%compile-form` function compiles Scheme to bytecode. Key sections:
- Data structures: lexical addresses, compile-time environments, constants tables
- Lap (bytecode assembly) generation functions (`%lap/var`, `%lap/save`, etc.)
- Special form expansion (let, letrec, cond, case, do, quasiquote)
- Primitive open-coding when `%open-code-primitives?` is true

### Initial heap in init.c

A serialized DAG encoded into literal bytes, deserialized at
startup. Supports: symbols, pairs, integers, booleans, strings, chars,
vectors, closures, refs to previous data. Integers use 7-bit varint
encoding with zigzag for signed values.

## Implementation Notes

- See user's guide Guide.md
- Redefining built-ins can potentially break the compiler
- Numeric tower: fixnums (62-bit) and IEEE doubles only

## Testing and Benchmarking

```bash
# Run R4RS test suite
test/run-tests

# Run corpus tests (real Scheme programs)
corpus/run-corpus

# Run benchmarks (tak, fib, ack, sum, sum-fp, fac, superopt, um)
./run-bench

# Save benchmark results for current commit
./run-bench --save

# Compare current benchmarks against baseline (flags >10% regression)
./compare-bench

# Set a commit as the benchmark baseline
./set-baseline [commit]
```

Benchmark results are stored in `bench-results/` with commit-stamped filenames. The baseline commit is tracked in `bench-results/baseline`.

**Benchmarking methodology:** Always rebuild with `make` (release mode) before benchmarking. Run multiple iterations and discard the first (cold cache). Be skeptical of large speedups from small changes - verify by A/B testing both versions in the same session.

**Corpus tests:** The `corpus/` directory contains real Scheme programs that exercise the interpreter more thoroughly than unit tests:
- **scheme-data-structures**: Queue, pairing heap, trie, sets, string matching
- **indent**: Indentation-based syntax parser and writer
- **consp**: Capability-secure language implementation
- **ridarama**: A* search transit planner (BART/Caltrain)
- **miasma**: x86 assembler generator

Tests compare output against reference files using `%system` to call external `diff`. Note: the interpreter checks most of its own work here, so bugs in core primitives could affect test reliability.

## 64-bit Port

The original code was written for 32-bit systems. Key changes for 64-bit:

- **62-bit fixnums**: Upgraded from 30-bit to use nearly all available bits
- **Object_header alignment**: Added padding to ensure 8-byte alignment of Object pointers stored after headers
- **Pointer-sized arithmetic**: Changed `object_bits()` to return `uintptr_t` and use `intptr_t` for pointer arithmetic in tag operations
- **Overflow detection**: Rewrote ADD/SUB to avoid signed overflow undefined behavior
- **round() conflict**: Renamed local `round()` to `my_round()` to avoid conflict with system library
