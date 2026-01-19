# Proposals

Potential improvements identified from code review. Priorities: keep codebase small and simple, avoid gratuitous fragility/inefficiency, improve R4RS compliance where practical.

## Architecture

### Global Error Recovery via longjmp

**Issue**: Error handling uses `longjmp` to a global `@error-cont`, which can leave the interpreter in an inconsistent state if an error occurs mid-operation (e.g., during GC allocation, mid-list construction).

**Risk**: Low in practice for interactive use, but makes the interpreter unsuitable for embedded/library use where errors should be recoverable.

**Possible fixes**:
- Document as known limitation
- Add explicit error-safe regions that defer longjmp
- Full rewrite with proper exception handling (major effort)

**Recommendation**: Document for now; fix only if embedding becomes a goal.

## Fragility

### Hardcoded Limits

Several limits are hardcoded without clear overflow handling:

- `MAX_STACK_SIZE` (4096 frames) - stack overflow just crashes
- Symbol table grows unboundedly with no size tracking
- String buffer sizes in reader

**Possible fixes**:
- Add graceful error messages when limits hit
- Make limits configurable via config.h
- Add dynamic resizing where feasible

### Symbol Table Size

Symbol interning uses a 101-bucket hash table. For very large programs with thousands of symbols, increasing the bucket count might help, but this is unlikely to be a bottleneck in practice.

### Compiler Warnings (Clang)

Building with clang and `-Wall` produces several warnings:

1. **Dangling else** (`-Wdangling-else`): In `read_token()` around lines 742/753. Adding explicit braces would silence these.

2. **Logical operator precedence** (`-Wlogical-op-parentheses`): Line 1177 has `a && b || c` without parentheses. Currently correct but fragile.

3. **Uninitialized variable** (`-Wsometimes-uninitialized`): Variable `b` in boolean literal parsing (line 1815) is uninitialized if neither `#t` nor `#f` is matched. The else branch calls `vm_error()` which doesn't return, but clang can't prove this.

4. **Volatile qualifier discarded** (`-Wincompatible-pointer-types-discards-qualifiers`): The `Interpreter i` is declared volatile for `setjmp` safety, but passed to functions expecting non-volatile pointers.

**Recommendation**: These are low priority - the code works correctly. Could silence with targeted fixes or `-Wno-*` flags if warnings become noisy.

## Efficiency

### String Operations

Many string operations (e.g., `string-append`) create intermediate objects that stress the GC.

**Recommendation**: Low priority unless profiling shows this is a bottleneck.

### Primitive Dispatch

The primitive dispatch table is searched linearly in some paths.

**Recommendation**: Current performance is acceptable per benchmarks; optimize only if profiling indicates need.

## R4RS Compliance

### Not Implemented (by design)

These are documented non-goals:

- **Hygienic macros** (`define-syntax`, `syntax-rules`): Would require significant compiler changes
- **Full numeric tower** (rationals, complex, bignums): Fixnum + flonum covers most practical use

### Deviations to Consider Fixing

- **`case` uses `eqv?`**: R4RS specifies `eqv?` for case clause matching; verify current implementation
- **`do` loop**: Verify step expressions are evaluated before bindings update
- **Quasiquote nesting**: Deep nesting of quasiquote/unquote may have edge cases

## Usability

### Tail Call Debugging

**Issue**: In debugging, tail calls overwrite the call frame, losing context about how the current point was reached. This is correct semantically but makes debugging difficult.

**Possible fixes**:
1. **Debug mode flag**: When enabled, disable tail-call optimization so full call stack is preserved
2. **Call trace buffer**: Keep a ring buffer of recent call sites even when frames are reused
3. **Conditional TCO**: Only optimize tail calls when not in debug/trace mode

**Recommendation**: Option 1 (debug flag to disable TCO) is simplest and matches user expectation that debugging may slow things down. Could be toggled via `(@debug-mode #t)` or similar.

### Error Messages

Some error messages could include more context:
- Source location (line/column) when available
- Expected vs actual types in type errors
- Procedure name in arity errors

**Recommendation**: Incremental improvement as pain points arise.

## Low Priority

These were noted but are not recommended for action:

- **Instruction encoding**: Current bytecode format works fine; redesign only if targeting size-constrained deployment
- **GC integration**: Boehm GC works well; custom GC only if pause times become problematic
- **Unicode**: R4RS doesn't require it; ASCII suffices for current use cases
