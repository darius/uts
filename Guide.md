# UTS User Guide

This guide covers just the particulars of `uts`, assuming you know
[R4RS][1]. If you're new to Scheme, I'm not up to date on any
tutorials; maybe start with [scheme.org][2].

 [1]: https://standards.scheme.org/official/r4rs.pdf
 [2]: https://www.scheme.org/

## OS command line

If you run `uts` with no arguments, it enters a read-eval-print loop
(REPL) -- see [Quick start][1].

 [1]: README.md#quick-start

If instead you run `uts filename.scm`, it loads and runs the Scheme
source from `filename.scm`. If an error occurs, it prints the
complaint and exits to the OS with status 1.

Any further command-line arguments are just made available to your
Scheme code (see `%command-line-arguments` below).

## Using the REPL

At the prompt `->` you enter an expression, then see its value:
```
-> (+ 2 3)
5
```

For some expressions, the value is unspecified by R4RS; for many of
these UTS returns a special symbol called `%void`, and the REPL
doesn't print it.

The last three non-`%void` values are kept handy in the variables `%`,
`%%`, and `%%%`.

There are some shortcut commands starting with a comma. For instance,
instead of entering `(load "foo.scm")` to load a source file:
```
-> ,l foo
-> 
```

`,help` will show the full list of commands.

To exit, enter the EOF character at your terminal (control-D in
Unix). Or enter `(%exit 0)`.

## Differences from R4RS

### Case sensitivity

Symbols and identifiers are case sensitive: `(eq? 'foo 'FOO)` is
false. Other Schemes like Chez and Guile make the same choice, though
R4RS forbids this, saying "For example, Foo is the same identifier as FOO."

### R4RS nonessentials omitted

Omitted numeric types: big integers (over 60 bits), rational numbers, complex numbers.

Their associated procedures:
- `numerator`, `denominator, `rationalize`
- `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`

Other nonessentials omitted:
- `with-input-from-file`, `with-output-to-file`
- `char-ready?`
- `transcript-on`, `transcript-off`
- the macro appendix

### Additions

Added functions will be listed below. They're generally named with a `%` prefix.

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

At the REPL, the `,d` command starts the debugger on the last
error. (Or call `(%debug)` for the same thing.)

In the debugger you explore the state at some point of a
computation. Most often you start with a backtrace (the `b` command)
showing the stack of pending non-tail calls, the most recent at
bottom. (This is different from the backtraces in many languages
because tail calls immediately drop the caller.) Then you might want
`a` for the bytecode assembly showing just where the last non-tail
call came from. `e` and `n` show the environment (variables and
values) at that call site. `?` lists more commands. `q` quits the
debugger.

You can drop into the debugger by choice in the middle of your code,
by calling `(%avast)` or `(%avast value)`. The latter will print out
the value before going into the debugger UI, and once you exit the
debugger it will return this value as the value of the call. (I wanted
a shorter word for this than 'breakpoint'.)

### `(%yo expression)`

A crude convenience macro for "printf debugging". `(%yo expression)`
evaluates just like `expression`, but also prints the value, like
this:
```
-> (define x 42)
x
-> (%yo x)
[%yo x : 42]
42
-> 
```

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

### `(%disassemble procedure)`

Like the debugger's `a` command, this shows the bytecode assembly for `procedure`.

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

### `(%cycle-write object)`

The basic `(write object)` has trouble with a circular data structure:
it never stops writing it. Use `cycle-write` instead to get a printed
representation rendering the circular references similarly to Common
Lisp. (TODO use this by default in the REPL and debugger?)

### `(%flush-input-line port)`

Discard characters until end of line. Useful for recovering from parse errors in interactive use.
