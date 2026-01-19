# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

UTS is a Scheme bytecode interpreter written in C, targeting R4RS compliance. It includes a compiler (written in Scheme) that compiles Scheme source to bytecode, and a runtime system with a REPL, debugger, and disassembler.

## Build Commands

```bash
# Build debug version (with assertions, much slower)
make

# Build fast/release version (no assertions)
./makefast

# Clean and rebuild
rm -f uts instruc-cases.c && make
```

## Dependencies

- **Boehm GC**: Required. Must be installed in `gc/` subdirectory with `gc.h` and `gc.a`
- **GCC**: Required for `inline` functions and 64-bit `long long int` support
- **AWK**: Required to generate `instruc-cases.c` from `instrucs.c`

## Running

```bash
# Start REPL
./uts uts.fasl

# Run a Scheme file
./uts uts.fasl -f program.scm arg1 arg2

# Rebuild uts.fasl from sources (from within REPL)
(load "write-fasl.scm")
(build-system "uts.scm" "uts.fasl")
```

## Bootstrap / Rebuilding uts.fasl

The compiled `uts.fasl` is needed to compile itself - it's a bootstrap dependency. When making changes that affect both `uts.c` and `uts.scm`:

1. **Simple changes**: Just rebuild fasl with `(load "write-fasl.scm") (build-system "uts.scm" "uts.fasl")`

2. **Changes to primitive names or C/Scheme interface**:
   - Keep old fasl working first (support both old and new names)
   - Rebuild fasl while old names still work
   - Then switch C to new names and rebuild C
   - Rebuild fasl again with new prim-lists: load uts.scm first to get updated prim-lists, then build

   ```bash
   # With proper prim-list update:
   ./uts uts.fasl -e '(load "write-fasl.scm") (load "uts.scm") (build-system "uts.scm" "uts.fasl")'
   ```

3. **If fasl becomes broken**: Restore from git with `git checkout HEAD -- uts.fasl`

## Architecture

### Core Components

- **uts.c**: Main C interpreter with bytecode VM, object representation, reader/writer, and garbage collection interface
- **instrucs.c**: Bytecode instruction implementations (processed by `make-instrucs.awk` into `instruc-cases.c`)
- **uts.scm**: Complete Scheme runtime - standard library, compiler, REPL, debugger, and disassembler (all in one file)
- **config.h**: Platform-specific configuration (word sizes, stack limits)

### Object Representation

Tagged pointers with low 2 bits encoding type:
- `00`: Boxed object pointer (flonum, port, string, pair, closure, symbol, vector)
- `01`: Fixnum (62-bit signed integer)
- `11`: Special values (chars, booleans, eof, nil, unbound)

Boxed objects have a header with size and 3-bit tag.

### Bytecode VM

23 instructions total. Key opcodes:
- `lit`, `varref`, `varset`, `global-ref/set/define`
- `if-false`, `jump`, `save`, `restore`, `invoke`, `apply`
- `proc`, `extend-normal-env`, `extend-&rest-env`
- `prim-0/1/2/3` (primitive calls with 0-3 args)
- `get-cc`, `set-cc` (continuations)

Continuation stack frames store: return PC, code vector, lexical environment, previous frame pointer.

### Compiler (in uts.scm)

The `parse-form` function compiles Scheme to bytecode. Key sections:
- Lexical environment handling (`lexical-env/lookup`, `lexical-env/extend`)
- Lap (bytecode assembly) generation functions (`lap/varref`, `lap/save`, etc.)
- Special form expansion (let, letrec, cond, case, do, quasiquote)
- Primitive open-coding when `*open-code-primitives?*` is true

### FASL Format

Binary format for compiled code with magic number `0xFADDF00D`. Version 4.0. Integers use 7-bit varint encoding with zigzag for signed values. Supports: symbols, pairs, integers, floats, booleans, strings, chars, vectors, code objects, closures.

## Key Global Variables (Scheme)

- `@command-line-args`: Command line arguments list
- `@error-cont`: Continuation for error recovery
- `@reset`: Continuation to restart REPL
- `@open-code-primitives?`: Controls primitive inlining (default `#t`)

## Debugging

From the REPL after an error:
```scheme
(debug)      ; Enter debugger
(@proceed v) ; Continue with value v
(dis proc)   ; Disassemble a procedure
```

Debugger commands: `?` help, `u` up, `d` down, `e` env, `n` next frame, `a` assembly, `s` stack, `b` backtrace, `q` quit.

## Implementation Notes

- Global definitions starting with `@` are internal (e.g., `@EVAL`, `@error`)
- Redefining standard procedures like `map` can break the compiler
- Numeric tower: fixnums (62-bit) and IEEE doubles only
- Macros (R4RS macro appendix) are not implemented
- Bitwise primitives: `bitwise-and`, `bitwise-ior`, `bitwise-xor`, `bitwise-not`, `arithmetic-shift`

## Testing and Benchmarking

```bash
# Run test suite (47 tests)
./run-tests

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

## 64-bit Port

The original code was written for 32-bit systems. Key changes for 64-bit:

- **62-bit fixnums**: Upgraded from 30-bit to use nearly all available bits
- **Object_header alignment**: Added padding to ensure 8-byte alignment of Object pointers stored after headers
- **Pointer-sized arithmetic**: Changed `object_bits()` to return `uintptr_t` and use `intptr_t` for pointer arithmetic in tag operations
- **Overflow detection**: Rewrote ADD/SUB to avoid signed overflow undefined behavior
- **round() conflict**: Renamed local `round()` to `my_round()` to avoid conflict with system library
