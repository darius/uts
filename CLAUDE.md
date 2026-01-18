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

## Architecture

### Core Components

- **uts.c**: Main C interpreter with bytecode VM, object representation, reader/writer, and garbage collection interface
- **instrucs.c**: Bytecode instruction implementations (processed by `make-instrucs.awk` into `instruc-cases.c`)
- **uts.scm**: Complete Scheme runtime - standard library, compiler, REPL, debugger, and disassembler (all in one file)
- **config.h**: Platform-specific configuration (word sizes, stack limits)

### Object Representation

Tagged pointers with low 2 bits encoding type:
- `00`: Boxed object pointer (flonum, port, string, pair, closure, symbol, vector)
- `01`: Fixnum (30-bit signed integer)
- `11`: Special values (chars, booleans, eof, nil, unbound)

Boxed objects have a 32-bit header with 29-bit size and 3-bit tag.

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

Binary format for compiled code with magic number `0xFADDF00D`. Version 4.0. Supports: symbols, pairs, integers, floats, booleans, strings, chars, vectors, code objects, closures.

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
- Numeric tower: fixnums (30-bit) and IEEE doubles only
- Macros (R4RS macro appendix) are not implemented

## 64-bit Compatibility

The original code was written for 32-bit systems. Key fixes for 64-bit:

- **Object_header alignment**: Added padding to ensure 8-byte alignment of Object pointers stored after headers (uts.c)
- **Pointer-sized arithmetic**: Changed `object_bits()` to return `uintptr_t` and use `intptr_t` for pointer arithmetic in tag operations (uts.c, instrucs.c, config.h)
- **highbit constant**: Changed from `(1 << 31)` to `(1UL << (WORD_BITS - 1))` for 64-bit (config.h)
- **round() conflict**: Renamed local `round()` to `my_round()` to avoid conflict with system library (uts.c, instrucs.c)
