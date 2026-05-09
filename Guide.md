# UTS User Guide

This guide assumes you know [R4RS][1], and covers the particulars of `uts`.

 [1]: https://standards.scheme.org/official/r4rs.pdf

## OS command line

If you run `uts` with no arguments, it enters a read-eval-print loop
(REPL) -- see Quick Start in README.md.

If instead you run `uts filename.scm`, it loads and runs the Scheme
source from `filename.scm`. If an error occurs, it prints the
complaint and exits to the OS with status 1.

Any further command-line arguments are just made available to your
Scheme code (see `%command-line-arguments` below).

## Using the REPL

At the prompt you can of course enter an expression and see its value:
```
-> (+ 2 3)
5
```

TODO document REPL features:
- ,commands
- ctrl-d to exit
- %, %%, %%% recent history
- `cycle-write`
- when value is `%void`, it's not written


## Differences from R4RS

Symbols and identifiers are case sensitive: `(eq? 'foo 'FOO)` is
false. Other Schemes like Chez and Guile make the same choice, though
R4RS contradicts it: it says "For example, Foo is the same identifier as FOO."

## R4RS nonessential features omitted

Omitted numeric types: big integers (over 60 bits), rational numbers, complex numbers.

Their associated procedures:
- `numerator`, `denominator, `rationalize
- `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`

Other nonessentials omitted:
- `with-input-from-file`, `with-output-to-file`
- `char-ready?`
- `transcript-on`, `transcript-off`
- the macro appendix

## Naming convention

Non-standard definitions should use a `%` prefix: `%error`, `%system`.

Some undocumented internals violate this convention. (XXX fixme)

## Error handling

### `(%error message . irritants)` / `(error message . irritants)`

Signal an error. Displays the message and any irritant values, then
calls `%reset`. The `error` name is provided as a SRFI-23 compatible
alias.

```scheme
(%error "file not found" filename)
(error "expected a number" x)  ; same thing
```

### `(%reset)`

What to do after `%error` complains to the user:

- If the REPL was running, then restart the REPL.
- If a command-line script was running, then exit the OS process with status 1 (meaning error).
- If the initial heap was still being built, then say so and panic (also an OS error status 1).

## Debugging

TODO proper documentation
- debugger
- %avast
- %yo

From the REPL after an error:

```scheme
(%debug)              ; Enter the debugger
(%disassemble proc)   ; Disassemble a procedure
```

Debugger commands: `?` help, `b` backtrace, `u`/`d` up/down frames, `e` show environment, `q` quit.

### `(%proceed value)`

Continue from the last error, returning `value` as the result of the expression that signaled the error.
(That last-error continuation was stashed in `%error-cont`.)

```scheme
-> (+ 1 (car 'not-a-pair))
[Error!] Expected pair
not-a-pair
-> (%proceed 42)
43
```

## Macros

You can define macros using `%define-macro`, though it's very
crude. See the system sources for examples and how it works. (Common
Lisp `defmacro` would be a natural next step if you like macros.)

Also some built-in functions: `%macroexpand-1`, etc.

## OS interface

### `%command-line-arguments`

A list of command-line arguments passed to UTS. The first element is the UTS executable, like argv[0] in C.

```scheme
-> %command-line-arguments
("uts" "program.scm" "arg1")
```

### `%arguments-to-scheme`

Like `%command-line-arguments` but without the UTS executable.

### `(%exit code)`

Exit the interpreter with the given status code (integer).

```scheme
(%exit 0)  ; success
(%exit 1)  ; failure
```

### `(%system command)`

Execute a shell command. Returns the exit status.

```scheme
(%system "ls -la")
(%system "gcc -o prog prog.c")
```

### `(%runtime)`

Returns the elapsed CPU time in seconds since the interpreter started (as a flonum).

```scheme
-> (%runtime)
0.042
```

## Evaluation

### `(%eval expression)`

Evaluate an expression in the global environment. This is the internal evaluator used by the REPL.

```scheme
(%eval '(+ 1 2))  ; => 3
```

### `(include "filename")`

Like `(load "filename")` with two differences:
- While `load` is a function which evaluates the contents of the file
  in the global environment, `include` instead is a macro which
  replaces itself with `(begin <contents-of-the-file>)`.
- `"filename"` must be a literal string, not a general expression.

## Bitwise Operations

These follow SRFI-60 naming (not prefixed with `%` since they're widely standard):

- `(bitwise-and n1 n2)` - Bitwise AND
- `(bitwise-ior n1 n2)` - Bitwise inclusive OR
- `(bitwise-xor n1 n2)` - Bitwise exclusive OR
- `(bitwise-not n)` - Bitwise complement
- `(arithmetic-shift n count)` - Shift left (positive count) or right (negative count)

All operate on fixnums only, i.e. 62-bit 2's-complement integers (in
the current implementation). Thus a bitwise op that messes with the
sign bit will return a nonportable result. `arithmetic-shift` can
overflow to a non-fixnum, in which case it will raise an error.

## Misc utilities

### `(%flush-input-line port)`

Discard characters until end of line. Useful for recovering from parse errors in interactive use.

