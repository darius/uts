# Corpus Notes

## Overview

5 projects, ~8,900 lines of Scheme code total:

| Project | Size | Description |
|---------|------|-------------|
| indent | 337 lines | Indentation-sensitive syntax parser |
| consp | 1,696 lines | Capability-secure Scheme interpreter |
| miasma | 4,039 lines | x86 machine code generator |
| ridarama | 2,166 lines | Transit trip planner with A* search |
| scheme-data-structures | 648 lines | Functional data structures library |

## Per-Project Details

### indent
Transforms Python-like indented syntax into S-expressions. Good for testing:
- Character-by-character I/O (`read-char`, `peek-char`)
- String operations
- Recursive descent parsing
- State machines

Entry: `loadme.scm`

### consp
A capability-secure language demo with interpreter, test suite, and example apps (money, voting, cryptographic protocols). Good for testing:
- Heavy use of lexical scoping (capability containment)
- Custom macro expansion (`defmacro` style)
- Mutation via boxes (`put!`/`get`)
- File I/O for bootstrapping

Entry: `loadme.scm`, tests in `tests.scm`

### miasma
Generates x86 assemblers in C and Python from instruction specifications. The largest and most complex project. Good for testing:
- Quasiquote/unquote extensively
- Custom macro system
- Bitwise operations
- Code generation (meta-programming)
- Complex nested data structures

Entry: `loadme.scm`, many test files in `src/test-*.scm`

### ridarama
Transit planner with BART schedules, geocoding, A* search. Good for testing:
- Record macros (`define-record` generates predicates, accessors, mutators)
- Variant/tagged unions with pattern matching
- Priority queues and search algorithms
- Time arithmetic
- Tries for string prefix matching

Entry: `loadme.scm`, tests in `fun-tests.scm`

### scheme-data-structures
Pure functional data structures: pairing heaps, tries, queues, sets. Good for testing:
- Purely immutable code (no `set!`)
- Higher-order functions with curried comparators
- Algorithmic correctness
- Can load 400KB wordlist for stress testing

Entry: load individual files

## UTS Testing Opportunities

### Currently exercised well
- Basic forms (lambda, let, letrec, define, cond, case, if, begin)
- Pairs/lists (cons, car, cdr, map, filter, append, reverse)
- Symbols and strings
- File I/O
- Higher-order functions

### Needs attention
1. **Macros**: All projects use `defmacro` style, but UTS doesn't have R4RS macros. Projects include their own macro expanders - need to verify these work.

2. **Quasiquote**: Miasma uses this heavily. Worth adding dedicated quasiquote tests.

3. **Bitwise operations**: Miasma needs `<<`, `>>`, etc. UTS has `arithmetic-shift`, `bitwise-and/ior/xor/not` - may need compatibility shims.

4. **`gensym`/`gentemp`**: Used by macro expanders. Verify UTS provides this.

5. **Large file I/O**: scheme-data-structures loads 400KB wordlist - good stress test.

6. **Deep recursion**: A* search and trie operations can go deep.

### Potential new test cases from corpus

1. **From scheme-data-structures**: Pairing heap and trie tests are self-contained and purely functional - easy to extract.

2. **From consp**: `tests.scm` has explicit test cases with expected outputs.

3. **From miasma**: `test-bits.scm`, `test-params.scm` etc. are modular unit tests.

4. **From ridarama**: Trie tests against `/usr/dict/words` (or `wordlist.txt`).

### Benchmark candidates

1. **Trie loading**: Load 400KB wordlist, do lookups
2. **A* search**: Path planning on transit graph
3. **Macro expansion**: Expand complex miasma code
4. **Pairing heap**: Insert/delete-min cycles

## Organization Thoughts

### Current state
All code is directly in `corpus/` as untracked files. This works but:
- Can't track upstream changes
- Unclear provenance
- Mixed with UTS development

### Options

**1. Keep as-is (simple)**
- Just add to `.gitignore` or commit directly
- Pro: Simple, no external dependencies
- Con: No upstream tracking, bloats repo

**2. Git submodules**
- If these have upstream repos, link them
- Pro: Track upstream, clear provenance
- Con: Submodules are awkward, may not have upstream repos

**3. Separate test-corpus repo**
- Move to `github.com/user/uts-test-corpus`
- Reference via submodule or script
- Pro: Clean separation
- Con: Extra repo to manage

**4. Vendor with attribution**
- Commit directly with clear README noting sources
- Pro: Self-contained, reproducible
- Con: Large commit, unclear if allowed by licenses

### Recommendation

Since these appear to be your own old projects (same author), simplest is:
1. Add a `corpus/README.md` documenting what each is
2. Commit them directly (they're small, ~2.4MB total)
3. Add a `corpus/run-all.sh` script that loads each and runs its tests
4. Don't worry about submodules unless there's an upstream to track

If you want to keep the main repo lean, put them in a sibling repo and add a script to fetch them.
