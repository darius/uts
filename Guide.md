# UTS User Guide

This guide documents non-standard extensions provided by UTS. Standard R4RS procedures are not covered here.

## Naming Convention

- Standard R4RS procedures use plain names: `car`, `map`, `call-with-current-continuation`
- Non-standard extensions for users use `%` prefix: `%error`, `%system`
- Internal implementation details use `@` prefix: `@make-code-vector`, `@driver-loop`

## Error Handling

### `(%error message . irritants)` / `(error message . irritants)`

Signal an error. Displays the message and any irritant values, then returns to the REPL. The `error` name is provided as a SRFI-23 compatible alias.

```scheme
(%error "file not found" filename)
(error "expected a number" x)  ; same thing
```

### `%error-cont`

The continuation captured when the last error occurred. Used internally by `%proceed`.

### `(%proceed value)`

Continue from the last error, returning `value` as the result of the expression that signaled the error. Only works if `%error-cont` is valid.

```scheme
-> (+ 1 (car 'not-a-pair))
[Error!] Expected pair
not-a-pair
-> (%proceed 42)
43
```

### `(%reset value)`

Restart the REPL. Usually called internally after errors.

## System Interface

### `%command-line-args`

A list of command-line arguments passed to UTS. The first element is the fasl filename.

```scheme
-> %command-line-args
("uts.fasl" "-f" "program.scm" "arg1")
```

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

## File Loading

### `(%load-fasl port-or-filename)`

Load compiled FASL code from a file or port. Used internally by the system.

### `(%read-fasl port)` / `(%write-fasl obj port)`

Low-level FASL I/O. Read or write a single FASL-encoded object.

### `(%read-fasl-header port)` / `(%write-fasl-header port)`

Read or write the FASL file header (magic number and version).

## REPL Utilities

### `(%flush-input-line port)`

Discard characters until end of line. Useful for recovering from parse errors in interactive use.

## Bitwise Operations

These follow SRFI-60 naming (not prefixed with `%` since they're widely standard):

- `(bitwise-and n1 n2)` - Bitwise AND
- `(bitwise-ior n1 n2)` - Bitwise inclusive OR
- `(bitwise-xor n1 n2)` - Bitwise exclusive OR
- `(bitwise-not n)` - Bitwise complement
- `(arithmetic-shift n count)` - Shift left (positive count) or right (negative count)

All operate on fixnums only.

## Debugging

From the REPL after an error:

```scheme
(debug)      ; Enter the debugger
(dis proc)   ; Disassemble a procedure
```

Debugger commands: `?` help, `b` backtrace, `u`/`d` up/down frames, `e` show environment, `q` quit.
