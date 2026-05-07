# UTS User Guide

This guide documents non-standard extensions provided by UTS. Standard R4RS procedures are not covered here.

## Naming Convention

- Standard R4RS procedures use plain names: `car`, `map`, `call-with-current-continuation`
- Non-standard definitions use `%` prefix: `%error`, `%system`

## Error Handling

### `(%error message . irritants)` / `(error message . irritants)`

Signal an error. Displays the message and any irritant values, then calls `%reset`. The `error` name is provided as a SRFI-23 compatible alias.

```scheme
(%error "file not found" filename)
(error "expected a number" x)  ; same thing
```

### `(%reset ignored-value)`

What to do after `%error` complains to the user:

- If the REPL was running, then restart the REPL.
- If a command-line script was running, then exit the OS process with status 1 (meaning error).
- If the initial heap was still being built, then say so and panic (also an OS error status 1).

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

## System Interface

### `%command-line-arguments`

A list of command-line arguments passed to UTS. The first element is the UTS executable, like argv[0] in C.

```scheme
-> %command-line-arguments
("uts.fasl" "-f" "program.scm" "arg1")
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

Compile and evaluate an expression. This is the internal evaluator used by the REPL.

```scheme
(%eval '(+ 1 2))  ; => 3
```

## Misc Utilities

### `(%flush-input-line port)`

Discard characters until end of line. Useful for recovering from parse errors in interactive use.

## Bitwise Operations

These follow SRFI-60 naming (not prefixed with `%` since they're widely standard):

- `(bitwise-and n1 n2)` - Bitwise AND
- `(bitwise-ior n1 n2)` - Bitwise inclusive OR
- `(bitwise-xor n1 n2)` - Bitwise exclusive OR
- `(bitwise-not n)` - Bitwise complement
- `(arithmetic-shift n count)` - Shift left (positive count) or right (negative count)

All operate on fixnums only. They don't raise an error on overflow. XXX arithmetic-shift should

## Debugging

From the REPL after an error:

```scheme
(debug)      ; Enter the debugger
(dis proc)   ; Disassemble a procedure
```

Debugger commands: `?` help, `b` backtrace, `u`/`d` up/down frames, `e` show environment, `q` quit.
