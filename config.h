// Parameters

#define VM_STACK_SIZE   32768
#define FASL_STACK_SIZE 1000


// Portable type definitions
//
// Word/UWord: pointer-sized integers for tagged pointer manipulation
// Fixnum: the integer type stored in tagged pointers (currently 30-bit)

#include <stdint.h>

typedef intptr_t  Word;      // signed pointer-sized integer
typedef uintptr_t UWord;     // unsigned pointer-sized integer
typedef int64_t   Fixnum;    // integer value in tagged fixnums

#define WORD_BITS    (sizeof(Word) * 8)
#define WORD_HIGHBIT ((UWord)1 << (WORD_BITS - 1))

// Fixnum range: 2 tag bits, so 62 bits of value on 64-bit
#define FIXNUM_BITS  62
#define FIXNUM_MIN   (-(1LL << (FIXNUM_BITS - 1)))
#define FIXNUM_MAX   ((1LL << (FIXNUM_BITS - 1)) - 1)
#define FIXNUM_MASK  FIXNUM_MAX

// Arithmetic right shift by 2 (for extracting fixnum value)
#define ashr2(w)     ((Word)(w) >> 2)

// Integer division - C99 specifies truncation toward zero
#define QUOTIENT(n,d)   ((n) / (d))
#define REMAINDER(n,d)  ((n) % (d))

#define UNPARSED_FLONUM_SIZE 1024

#define fast static inline

// Legacy type aliases - to be removed
#define int32      int32_t
#define unsigned32 uint32_t
#define int64      int64_t
