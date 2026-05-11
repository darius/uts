UTS is a Scheme system I wrote in the 1990s and dropped a few years
later, now being overhauled.

It's a bytecode interpreter in C plus a compiler to the bytecode in
Scheme. The compiler compiles itself into a C byte array which is then
included into the C source. You need another Scheme system for this
initial bootstrapping. (Once you have it, you can drop the other
Scheme and use UTS to develop itself.)

It conforms to R4RS (mostly), omitting optional features like the
macro appendix.


## Installing

First install the [Boehm garbage collector][1].

 [1]: https://www.hboehm.info/gc/

You'll need a standard Scheme preinstalled to bootstrap the bytecode
compiler into C. [Chez Scheme][2] or [Guile][3] should just work. For another
Scheme, edit `build-init` to give the executable name. You may need to
also edit the top of `load-init.scm` to supply a few nonportable
Scheme functions. Chez and Guile have a nonstandard Scheme function
`include` which this bootstrap depends on; `(define include load)`
might work on other Schemes, but I haven't tried that.

 [2]: https://cisco.github.io/ChezScheme/
 [3]: https://www.gnu.org/software/guile/

The C code currently assumes 64 bits and a few other things more
modern than the 90s original. Will document/improve later.

The build process also uses `awk`.

Given these dependencies, do
`./configure && make && make install`.

There are some tests once it's built:
`make test && ./run-bench && corpus/run-corpus`.

If the testing fails, `make DEBUG=1` may be more tractable to debug (but runs
much slower).


## Quick start

```
$ uts
Enter an expression. On an error, enter ,d to debug. For more commands: ,help
-> (+ 2 3)
5
-> ,help
,help        - this message
,d           - debug the last error, i.e. (%debug)
,l name      - (load "name.scm")
,l "x.scm"   - (load "x.scm")
,! expr      - evaluate expr for effect, don't print it
,time expr   - time the evaluation of expr
-> oops

[Error!] Unbound variable
oops
-> ,d
Enter ? for help.
debug> ?
? help      - this message
q quit      - quit the debugger
b backtrace - names of the current procedure and its callers
a assembly  - show assembly source of the current procedure
e env       - show the inner frame of the current environment
n next      - show the next frame of the current environment
s stack     - show the local value stack
u up        - up to caller
d down      - down to callee
debug> b
%scheming
#f
debug> a
    0 glo oops
=>  2 restore
debug> q
-> [ctrl-d to exit]
$ 
```

## Learn more

See [Guide.md](Guide.md).


## Why care?

Beats me! At least it's pretty small. There's a little more motivation
in the [original README](README.old.text). Goal for the overhaul: to
be nicer to read, and maybe handy as a tractable personal programming
environment.


## How could this suck less?

These would be nice:
- Some reasonable way to intercept errors you want to intercept.
- Source-level debugging.
- More-helpful errors from primitive procedures.
- A precise GC in place of the conservative Boehm collector.


## Authorship

- Initial resurrection in 2026: Claude Opus 4.5. Fixed undefined
  behavior shown up by newer C compilers, converted to 64 bits,
  overhauled configure/build/install, added tests and profiling,
  other small improvements, helped with perf. Thanks, Claude!

- Before and after: me, Darius
