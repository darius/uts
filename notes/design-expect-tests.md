# Design: Inline Expect Tests for UTS

## Motivation

Support a development workflow where:
- You write expressions in a buffer
- Hit a key, all results appear inline
- Results serve as both exploration output and regression tests
- Minimal ceremony, close to REPL feel

Secondary goal: support both human interactive use and automated testing (including by an AI assistant working through pipes).

## Core Model

### Chunks

The buffer is a sequence of **source lines** interleaved with **result blocks**.

```scheme
(define (f x) (* x 2))    ; source line

(f 3)                     ; source line
;> 6                      ; result block (one line)

(map f '(1 2 3))          ; source line
;> (2                     ; result block
;>  4                     ; (multiple lines)
;>  6)

(car 'oops)               ; source line
;!> [Error] Bad type      ; result block (error)
;!> oops
```

- Source lines: regular Scheme code/comments
- Result blocks: consecutive lines starting with `;>` (value) or `;!>` (error)
- A result block attaches to the preceding source region

This model survives incomplete/broken syntax during editing - chunks are text units, not parsed expressions.

### Evaluation

1. Strip all result blocks, keeping their positions
2. Read and eval the source text top-to-bottom, fresh environment
3. Each top-level expression produces a result (value or error)
4. First error halts evaluation
5. Results attach to positions in the source

### Modes

**Update mode**: Eval and rewrite result blocks with actual results.

**Verify mode**: Eval, compare results, report mismatches, exit nonzero on failure.

### Multi-line Results

Each line prefixed with `;>` or `;!>`. Simple, robust, no special delimiter needed.

```scheme
(list 1 2 3)
;> (1
;>  2
;>  3)
```

### Errors as Results

```scheme
(car 5)
;!> [Error] Bad type
;!> 5
```

Shows error message and irritants. Error stops further evaluation.

### I/O Output

Stdout is captured automatically if anything is written:

```scheme
(+ 1 2)
;> 3

(begin (display "hi\n") 42)
;>> hi
;> 42
```

The `;>>` lines appear when there's output, absent when there isn't. No marker needed to request capture - the result shows what happened.

## What's NOT Supported

### Inline Sub-expression Results

```scheme
(+ (foo 3) ;> 6    ; NOT SUPPORTED
   (bar 4))
;> 10
```

Result markers only at top-level. For intermediate values, use explicit tracing:

```scheme
(define-syntax yo
  (syntax-rules ()
    ((yo expr)
     (let ((v expr))
       (display "yo: ")
       (write 'expr)
       (display " = ")
       (write v)
       (newline)
       v))))

(+ (yo (foo 3)) (bar 4))
;>> yo: (foo 3) = 6
;> 10
```

This keeps the core simple. Different tools for different jobs.

### Debug Channel

A separate debug port for chatter that shouldn't appear in test results:

```scheme
(btw "checkpoint reached, x=~w" x)  ; goes to debug log, not captured
```

Nice to have, not essential initially.

## Core Requirements

What the interpreter needs to support this:

### Essential

1. **Autoflush on newline** - Output must flush predictably so timeout/hang detection works. The announce-before-eval pattern depends on this.

2. **Clean exit codes** - `(exit 0)` and `(exit 1)` for scripting.

3. **Error hook or handler** - Ability to catch errors programmatically rather than dropping to REPL:
   ```scheme
   (with-error-handler
     (lambda (msg irritants) ...)
     (lambda () (load "file.scm")))
   ```
   Or simpler: `@error-hook` variable that gets called.

### Nice to Have

4. **Timeout primitive** - `(with-timeout ms thunk)` that returns `#f` or error on timeout. Would allow per-expression timeouts without external wrapper. Complex to implement (needs VM interruptibility).

Note: `random` is not a core requirement - can be implemented in pure Scheme (see `random.scm`).

## Implementation Phases

### Phase 1: Test Script (External)

A shell script wrapper:

```bash
#!/bin/bash
timeout 30 ./uts uts.fasl -f run-expect.scm "$1"
```

The Scheme side (`run-expect.scm`):
- Reads file, strips result blocks
- Evals each expression, capturing result
- Compares or updates result blocks
- Prints structured output:
  ```
  CHUNK: 3
  PASS: 3
  CHUNK: 5
  FAIL: 5
    expected: 6
    actual:   7
  ```
- Exits nonzero on any failure

Timeout catches hangs. Last `CHUNK:` line identifies the culprit.

### Phase 2: Editor Integration

The terminal editor (future) has a keystroke that:
1. Saves buffer to temp file
2. Runs the test script in update mode
3. Reads back the updated file
4. Replaces buffer contents

Or eventually, tighter integration where the editor runs eval directly.

### Phase 3: Richer Features

- Subsetting (run only some chunks)
- Floating-point tolerance
- Pattern matching for error messages
- Debug port capture
- Property-based testing

## For Claude (AI Tester)

What I need for effective automated testing:

1. **Structured output** - Parseable `PASS:`/`FAIL:` lines, not prose.

2. **Exit codes** - Nonzero on failure so scripts detect problems.

3. **Clear failure context** - Show expected, actual, and the expression.

4. **Run subsets** - Ability to run specific chunks or files matching a pattern, for faster iteration.

5. **No interactive prompts** - Everything works via pipes and files.

The current `run-tests` script works fine for pass/fail. The expect-test format would additionally let me:
- Write exploratory code, stamp results as tests
- See exactly what changed when something breaks
- Update expectations when behavior intentionally changes

### Alternative Models?

The inline expect-test model is good. Alternatives I've used:

- **Separate test files with assertions** - Current `test.scm` style. Fine, but more friction to write.
- **Golden file comparison** - Run program, diff output against saved file. Coarser grained.
- **Property-based testing** - Great for finding edge cases, but needs generators and shrinking. More infrastructure.

For this project's spirit of "close to basics," the inline expect-test approach hits a sweet spot: minimal infrastructure, maximum utility, natural REPL-like workflow.

## Minimal Starting Point

To get something working quickly:

1. Add `@error-hook` to core (few lines in `uts.scm`)
2. Write `expect.scm`:
   - `(run-expect-file "foo.scm" 'verify)`
   - `(run-expect-file "foo.scm" 'update)`
3. Write wrapper script with timeout
4. Convert some existing tests to expect format as proof of concept

The core stays minimal. The testing infrastructure is pure Scheme.

## Open Questions

- Exact syntax for error results (how much of the error to capture?)
- How to handle non-deterministic output (timestamps, addresses)
- Whether result blocks should preserve old value as comment when updating
- Chunk numbering: by source line? by expression index? by explicit label?

## Deferred Issues

- **`;>` inside code blocks**: A `;>` inside a string or quoted list isn't a result marker, e.g. `(define s ";> not a result")`. The chunk parser needs to track nesting rather than just line-by-line pattern matching.

## References

- [halp](https://github.com/darius/halp) - Prior art by same author
- [Jane Street ppx_expect](https://github.com/janestreet/ppx_expect) - OCaml expect tests
- [The Joy of Expect Tests](https://blog.janestreet.com/the-joy-of-expect-tests/)
