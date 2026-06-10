#include <gc.h>
#include "config.h"

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "opcodes.h"

static clock_t clock_start;     // needed by (%runtime)


typedef enum { false=0, true=1 } Flag;

typedef unsigned char Char;

typedef enum {
  a_flonum, an_input_port, an_output_port, a_string,
  a_pair, a_closure, a_symbol, a_vector,
  num_tags                    // not a real tag
} Tag;


fast Flag
int_is_fixnum(Fixnum i) {
  // Unsigned comparison trick: (i - MIN) <= (MAX - MIN)
  return (UWord)(i - FIXNUM_MIN) <= (UWord)(FIXNUM_MAX - FIXNUM_MIN);
}


enum { obj_size_bits = 29, obj_size_limit = 1<<obj_size_bits };

typedef struct Object_header {
  unsigned size      : obj_size_bits;
  unsigned tag       : 3;   // if any more types, have to expand to 4 bits
#if __SIZEOF_POINTER__ > 4
  unsigned _pad;            // padding for 64-bit alignment
#endif
} Object_header;

typedef Object_header *Object;  // A Scheme datum, or pointer thereto.
                                // The lower bits are tag bits, as follows:
// xxxxxxxx xxxxxxxx xxxxxxxx xxxxxx00   pointer
// xxxxxxxx xxxxxxxx xxxxxxxx xxxxxx01   fixnum
// xxxxxxxx xxxxxxxx xxxxxxxx xxxxxx10   reserved
// 00000000 00000000 0000xxxx xxxx0011   char
// 00000000 00000000 00000000 00x01011   boolean
// 00000000 00000000 00000000 0xx11011   00=eof, 01=(), 10=unbound
// xxxxxxxx xxxxxxxx xxxxxxxx xxxxx111   reserved

// A pointer Object points to an Object_header, with the object's data
// directly following the header in memory.

fast UWord
object_bits(Object obj) {
  return (UWord) obj;
}


static void
fatal_error(const char *message) {
  fprintf(stderr, "Fatal error: %s\n", message);
#ifndef NDEBUG
  __builtin_trap();
#endif
  exit(1);
}

static void 
unreachable(void) {
  fatal_error("Unreachable");
}

// N.B. these would have to be thread-local
static jmp_buf          vm_error_catch_point;
static const char *     vm_error_message;
static Object           vm_error_irritant;

static void 
signal_vm_error(void) {
  longjmp(vm_error_catch_point, 1);
}

static void 
vm_error(const char *message, Object irritant) {
  vm_error_message  = message;
  vm_error_irritant = irritant;
  longjmp(vm_error_catch_point, 1);
}

#ifndef NDEBUG
fast void 
assert(Flag flag) { 
  if (!flag)
    fatal_error("Can't happen");
}
#else
#define assert(flag)
#endif


fast Flag 
is_boxed(Object obj) { 
  return (object_bits(obj) & 0x03) == 0x00; 
}

fast Object_header 
object_header(Object obj) {
  assert(is_boxed(obj));
  return *(Object_header *) obj;
}

fast Tag 
object_tag(Object obj) {
  assert(is_boxed(obj));
  return object_header(obj).tag;
}

fast void *
data_ptr(Object obj) {
  assert(is_boxed(obj));
  return (char *) obj + sizeof(Object_header);
}

fast Tag 
tag(Object obj)            { return object_header(obj).tag; }


#define obj_eof          ( (Object) 0x0000001b )
#define nil              ( (Object) 0x0000003b )
#define unbound          ( (Object) 0x0000005b )
#define obj_false        ( (Object) 0x0000000b )
#define obj_true         ( (Object) 0x0000002b )

fast Flag is_eof_object(Object obj) { return obj == obj_eof; }
fast Flag is_null(Object obj)       { return obj == nil; }
fast Flag is_unbound(Object obj)    { return obj == unbound; }


fast Flag is_boolean(Object obj)   {return (object_bits(obj) & 0x1f) == 0x0b;}
fast Flag is_char(Object obj)      {return (object_bits(obj) & 0x0f) == 0x03;}
fast Flag is_fixnum(Object obj)    {return (object_bits(obj) & 0x03) == 0x01;}


fast Flag is_true(Object obj)       { return obj != obj_false; }
fast Object make_boolean(Flag flag) { return flag ? obj_true : obj_false; }

fast Object make_char(Char c)       { return (Object) (((UWord)c << 4) | 0x03); }
fast Char char_value(Object obj)    { assert(is_char(obj));
                                      return (Char) (object_bits(obj) >> 4); }

fast Object make_fixnum(Fixnum n)   { assert(int_is_fixnum(n));
                                      return (Object) (((UWord)n << 2) | 0x01); }
fast Fixnum fixnum_value(Object obj){ assert(is_fixnum(obj));
                                      return ashr2(object_bits(obj)); }


fast Flag is_input_port(Object obj) { return is_boxed(obj)
                                        && tag(obj) == an_input_port; }
fast Flag is_output_port(Object obj){ return is_boxed(obj)
                                        && tag(obj) == an_output_port; }

fast Flag is_flonum(Object obj) {return is_boxed(obj) && tag(obj)==a_flonum;}
fast Flag is_string(Object obj) {return is_boxed(obj) && tag(obj)==a_string;}
fast Flag is_pair  (Object obj) {return is_boxed(obj) && tag(obj)==a_pair;}
fast Flag is_closure(Object obj){return is_boxed(obj) && tag(obj)==a_closure;}
fast Flag is_symbol(Object obj) {return is_boxed(obj) && tag(obj)==a_symbol;}
fast Flag is_vector(Object obj) {return is_boxed(obj) && tag(obj)==a_vector;}

fast Flag 
is_number(Object obj) {
  return is_fixnum(obj) || is_flonum(obj);
}

fast Flag
is_natnum(Object obj) {   // tag trickery 
  return (object_bits(obj) & (WORD_HIGHBIT | 0x03)) == 0x01;
}


static void 
type_error(Object obj) {
  vm_error("Bad type", obj);
}

static void 
range_error(Fixnum i) {
  vm_error("Range error", make_fixnum(i));
}

// is a call to this from here really a good idea?
static Object c_string(const char *string);

static void
io_error(int errno_code) {
  vm_error("I/O error", c_string(strerror(errno_code)));
}

static void 
heap_error(void) {
  fatal_error("Out of heap space");
}

static void 
size_error(void) {
  fatal_error("Allocation size overflow");
}

fast void 
check_type(Flag flag, Object obj) { 
  if (!flag) 
    type_error(obj);
}

// TODO review size argument type for all callers of allot/allot_atomic
fast Object 
allot(Tag tag, size_t size) {
  assert(0 <= tag && tag < num_tags);
  if (obj_size_limit <= size) size_error();
  Object_header *header = (Object_header*)GC_malloc(sizeof *header + size);
  if (header == NULL)
    heap_error();
  header->size = size;
  header->tag = tag;
  return header;
}

fast Object 
allot_atomic(Tag tag, size_t size) {
  assert(0 <= tag && tag < num_tags);
  if (obj_size_limit <= size) size_error();
  Object_header *header = (Object_header*)GC_malloc_atomic(sizeof *header + size);
  if (header == NULL)
    heap_error();
  header->size = size;
  header->tag = tag;
  return header;
}

// TODO changing this to Fixnum seems to slow us down a bit measurably. true? what to do?
fast unsigned
vector_length(Object vec) {
  assert(is_vector(vec));
  return object_header(vec).size / sizeof(Object);
}

fast Object *
vector_ptr(Object obj) {
  assert(is_vector(obj));
  return data_ptr(obj);
}

fast unsigned 
string_length(Object str) {
  assert(is_string(str));
  return object_header(str).size;
}

fast Char *
string_ptr(Object obj) {
  assert(is_string(obj));
  return data_ptr(obj);
}

fast const char *
string_cstr(Object obj) {
  return (const char *)string_ptr(obj);
}

static Flag 
string_equal(Object str1, Object str2) {
  assert(is_string(str1));
  assert(is_string(str2));
  unsigned L = string_length(str1);
  if (string_length(str2) != L)
    return false;
  const Char *s1 = string_ptr(str1);
  const Char *s2 = string_ptr(str2);
  for (unsigned i = 0; i < L; ++i)
    if (s1[i] != s2[i])
      return false;
  return true;
}


struct File_port {
  FILE *file;
  Flag is_open;
};

fast struct File_port *
port_ptr(Object port) {
  assert(is_port(port));
  return (struct File_port *) data_ptr(port);
}

fast FILE *
port_file(Object port) {
  return port_ptr(port)->file;
}

fast Flag
port_is_open(Object port) {
  return port_ptr(port)->is_open;
}

fast void
check_openness(Object port) {
  if (!port_is_open(port))
    vm_error("Access to closed port", port);
}

static void 
close_port(Object port) {
  struct File_port *fp = port_ptr(port);
  if (fp->is_open)              // if not, is that an error?
    fclose(fp->file);
  fp->is_open = false;
}

static void
port_finalizer(GC_PTR port_obj, GC_PTR _) {
  (void)_;
  close_port((Object) port_obj);
}

static Object 
make_port(Tag tag, FILE *file) {
  Object result = allot_atomic(tag, sizeof(struct File_port));
  port_ptr(result)->file = file;
  port_ptr(result)->is_open = true;
  GC_register_finalizer(result, port_finalizer, NULL, NULL, NULL);
  return result;
}

// RECOVERABLE
static void 
object_to_c_string(char *buffer, size_t size, Object str) {
  size_t L = string_length(str);
  if (size < L + 1)
    vm_error("Buffer overflow while converting to C string", str);
  else {
    char *d = buffer;
    for (const char *s = string_cstr(str); L != 0; ++s, ++d, --L)
      *d = *s;
    *d = '\0';
  }
}

static char *
string_to_c(Object string) {
  assert(is_string(string));
  int size = 1 + string_length(string);
  char *s = (char *) GC_malloc_atomic(size);
  if (s == NULL)
    heap_error();
  object_to_c_string(s, size, string);
  return s;
}

// RECOVERABLE
static Object 
open_file(Object filename, Tag tag, const char *mode) {
  char name[FILENAME_MAX];
  object_to_c_string(name, sizeof name, filename);
  FILE *file = fopen(name, mode);
  if (!file)
    io_error(errno);
  return make_port(tag, file);
}
  

static Object void_marker;
static Object current_input_port, current_output_port;
static Object halt_code, just_invoke_code, reified_cont_code;
static Object code_vector_symbol;
static Object global_renv;
static Object symbol_table;

fast double 
flonum_value(Object n) {
  assert(is_flonum(n));
  return *(double *) data_ptr(n);
}

fast Object 
make_flonum(double n) {
  Object result = allot_atomic(a_flonum, sizeof n);
  *(double *) data_ptr(result) = n;
  return result;
}

fast Flag 
eqv(Object obj1, Object obj2) {
  return obj1 == obj2
   || (is_flonum(obj1) && is_flonum(obj2) 
       && flonum_value(obj1) == flonum_value(obj2));
}

static double
my_round(double x) {
  double ignore, i, f = modf(x, &i);
  if (f < 0) {
    if (f < -0.5 || (f == -0.5 && modf(i * 0.5, &ignore) != 0))
      i -= 1;
  } else {
    if (0.5 < f || (f == 0.5 && modf(i * 0.5, &ignore) != 0))
      i += 1;
  }
  return i;
}


// TODO name with _unsafe
fast Object 
field_ref(Object obj, size_t index) {   // XXX s/size_t/Fixnum ?
  assert(is_boxed(obj));
  assert(index < object_header(obj).size / sizeof(Object));
  return ((Object *) data_ptr(obj))[index];
}

fast void 
field_set(Object obj, size_t index, Object value) {   // XXX s/size_t/Fixnum ?
  assert(is_boxed(obj));
  assert(index < object_header(obj).size / sizeof(Object));
  ((Object *) data_ptr(obj))[index] = value;
}


fast Object 
allot2(Tag tag, Object obj0, Object obj1) {
  Object result = allot(tag, sizeof obj0 + sizeof obj1);
  field_set(result, 0, obj0);
  field_set(result, 1, obj1);
  return result;
}

static void
vector_size_error(Fixnum length) {
  vm_error("Can't make a vector of out-of-range length", make_fixnum(length));
}

fast Object 
allot_vector(Fixnum length) {
  if (obj_size_limit / sizeof(Object) <= (UWord)length)
    vector_size_error(length);
  return allot(a_vector, ((UWord)length) * sizeof(Object));
}

static void
vector_index_error(Fixnum index) {
  vm_error("Vector access out of range", make_fixnum(index));
}

fast Object 
vector_ref(Object vec, Fixnum index) {
  assert(is_vector(vec));
  if (vector_length(vec) <= (UWord)index)
    vector_index_error(index);
  return field_ref(vec, index);
}

fast Object 
vector_ref_unsafe(Object vec, Fixnum index) {
  assert(is_vector(vec));
  assert((UWord)index < vector_length(vec));
  return field_ref(vec, (size_t) index);
}

fast void 
vector_set(Object vec, Fixnum index, Object value) {
  assert(is_vector(vec));
  if (vector_length(vec) <= (UWord)index)
    vector_index_error(index);
  field_set(vec, index, value);
}

fast void 
vector_set_unsafe(Object vec, Fixnum index, Object value) {
  assert(is_vector(vec));
  assert((UWord)index < vector_length(vec));
  field_set(vec, index, value);
}

static void 
vector_fill(Object vec, Object filler) {
  assert(is_vector(vec));
  Fixnum limit = vector_length(vec);
  for (Fixnum i = 0; i < limit; ++i)
    vector_set_unsafe(vec, i, filler);
}

fast Object 
make_vector(Fixnum length, Object filler) {
  Object vec = allot_vector(length);
  vector_fill(vec, filler);
  return vec;
}


fast Object 
cons(Object the_car, Object the_cdr) {
  return allot2(a_pair, the_car, the_cdr);
}

fast Object 
make_closure(Object renv, Object code_vector) {
  assert(is_vector(renv));
  assert(is_vector(code_vector));
  return allot2(a_closure, renv, code_vector);
}

fast Object
closure_renv(Object closure) {
  assert(is_closure(closure));
  return field_ref(closure, 0);
}

fast Object
closure_code(Object closure) {
  assert(is_closure(closure));
  return field_ref(closure, 1);
}

static Object 
c_string(const char *string) {
  int l = strlen(string);
  Object s = allot_atomic(a_string, l);
  memcpy(string_ptr(s), string, l);
  return s;
}

fast Object
make_string(unsigned length) {
  return allot_atomic(a_string, length);
}


// --- stuff ---

fast Object 
car(Object pair) {
  assert(is_pair(pair));
  return field_ref(pair, 0); 
}

fast Object 
cdr(Object pair) {
  assert(is_pair(pair));
  return field_ref(pair, 1); 
}

// RECOVERABLE
static int                      // unsigned?
list_length(Object list) {
  Object ls = list;
  int l = 0;
  for (; is_pair(ls); ls = cdr(ls))
    ++l;
  if (!is_null(ls))
    vm_error("Improper list", list);
  return l;
}

// RECOVERABLE
static Object
list_reverse(Object list) {
  Object rest = list;
  Object reversed = nil;
  for (; is_pair(rest); rest = cdr(rest))
    reversed = cons(car(rest), reversed);
  if (!is_null(rest))
    vm_error("Improper list", list);
  return reversed;
}

// RECOVERABLE
static Object
assq(Object key, Object list) {
  Object rest = list;
  for (; is_pair(rest); rest = cdr(rest)) {
    Object pair = car(rest);
    if (!is_pair(pair))
      vm_error("Bad association list", list);
    if (key == car(pair))
      return pair;
  }
  if (!is_null(rest))
    vm_error("Improper list", list);
  return obj_false;
}

// RECOVERABLE
static Object
assv(Object key, Object list) {
  Object rest = list;
  for (; is_pair(rest); rest = cdr(rest)) {
    Object pair = car(rest);
    if (!is_pair(pair))
      vm_error("Bad association list", list);
    if (eqv(key, car(pair)))
      return pair;
  }
  if (!is_null(rest))
    vm_error("Improper list", list);
  return obj_false;
}

// RECOVERABLE
static Object
memq(Object key, Object list) {
  Object rest = list;
  for (; is_pair(rest); rest = cdr(rest)) 
    if (key == car(rest))
      return rest;
  if (!is_null(rest))
    vm_error("Improper list", list);
  return obj_false;
}

// RECOVERABLE
static Object
memv(Object key, Object list) {
  Object rest = list;
  for (; is_pair(rest); rest = cdr(rest)) 
    if (eqv(key, car(rest)))
      return rest;
  if (!is_null(rest))
    vm_error("Improper list", list);
  return obj_false;
}

// RECOVERABLE
// FIXME: code this more efficiently
static Object
append(Object list1, Object list2) {
  if (is_pair(list1))
    return cons(car(list1), append(cdr(list1), list2));
  if (!is_null(list1))
    vm_error("Improper list", list1);
  return list2;
}

// RECOVERABLE
static Object
list_to_vector(Object list) {
  Object vec = make_vector(list_length(list), obj_false);
  Fixnum i = 0;
  for (Object rest = list; is_pair(rest); rest = cdr(rest), ++i) 
    vector_set_unsafe(vec, i, car(rest));
  return vec;
}

fast void 
put_char(int c, FILE *out) {
again:
  if (EOF == putc(c, out)) {
    if (errno == EINTR) goto again;
    io_error(errno);
  }
}

fast void 
put_string(const char *s, FILE *out) {
again:
  if (EOF == fputs(s, out)) {
    if (errno == EINTR) goto again;
    io_error(errno);
  }
}

// RECOVERABLE
static void
display_string(Object str, FILE *out) {
  assert(is_string(str));
  Fixnum sl = string_length(str);
  const Char *s = string_ptr(str);
  for (Fixnum i = 0; i < sl; ++i)
    put_char(s[i], out);
}

fast Object 
global_value(Object sym)  {
  assert(is_symbol(sym));
  return field_ref(sym, 0);
}

fast void
set_global_value(Object sym, Object value) {
  assert(is_symbol(sym));
  field_set(sym, 0, value);
}

// N.B. copy the returned string if there's any chance of it being mutated
fast Object 
symbol_to_string(Object sym) {
  assert(is_symbol(sym));
  return field_ref(sym, 1); 
}

static Object
make_symbol(Object str) {
  assert(is_string(str));
  return allot2(a_symbol, unbound, str);
}

static Object
string_copy(Object str) {
  assert(is_string(str));
  unsigned l = string_length(str);
  Object str2 = allot_atomic(a_string, l);
  memcpy(string_ptr(str2), string_ptr(str), l);
  return str2;
}

static UWord
string_hash(Object str) {
  assert(is_string(str));
  const Char *s = string_ptr(str);
  UWord acc = 0;
  for (Fixnum l = string_length(str); l != 0; ++s, --l)
    acc = acc * 2 + *s;
  return acc;
}

static Object
string_to_symbol(Object str) {
  assert(is_string(str));
  assert(is_vector(symbol_table));
  Fixnum i = string_hash(str) % (UWord)vector_length(symbol_table);
  Object bucket = vector_ref(symbol_table, i);
  for (Object syms = bucket; is_pair(syms); syms = cdr(syms)) {
    Object a = car(syms);
    assert(is_symbol(a));
    if (string_equal(str, symbol_to_string(a)))
      return a;
  }
  // TODO: optimize out this string_copy where you can...
  Object sym = make_symbol(string_copy(str));
  vector_set(symbol_table, i, cons(sym, bucket));
  return sym;
}

static Object
c_symbol(const char *s) {
  return string_to_symbol(c_string(s));
}

static Object
make_code_vector(Object data, Object bytecodes, Object label, Object locals_map) {
  assert(is_vector(data));
  assert(is_string(bytecodes));
  Object codevec = allot_vector(6);
  Object *cv = vector_ptr(codevec);
  cv[0] = code_vector_symbol;
  cv[1] = data;
  cv[2] = bytecodes;
  cv[3] = label;
  cv[4] = locals_map;
  cv[5] = make_fixnum(0);
  return codevec;
}

// RECOVERABLE
// The name is misleading: it also skips Lisp comments.
static void
skip_blanks(FILE *in) {
  int c = getc(in);
  for (;;) {
    while (isspace(c))
      c = getc(in);
    if (c != ';')
      break;
    do
      c = getc(in);
    while (c != '\n' && c != EOF);
  }

  if (c == EOF && ferror(in)) 
    vm_error("I/O error on input", nil); // need the port!
  ungetc(c, in);
}

// RECOVERABLE
static void 
flush_input_line(FILE *in) {
  for (;;) {
    int c = getc(in);
    if (c == '\n')
      return;
    if (c == EOF) {
      if (ferror(in))
        vm_error("I/O error on input", nil); // need the port!
      return;
    }
  }
}

// Pre: 2 <= radix <= 36
static int
convert_digit(char c, int radix) {
  int digit;
  if (c < '0')
    return -1;
  if (c <= '9')
    digit = c - '0';
  else {
    char C = toupper(c);
    if (C < 'A')
      return -1;
    digit = C - ('A' - 10);
  }
  return digit < radix ? digit : -1;
}

static Object 
string_to_number(Object str, unsigned radix) {
  if (radix < 2 || 36 < radix)
    return obj_false;   // TODO or should we raise an error?

  // The grammar for real numbers, where R is the radix, from R4RS:

  // num[R]:            whitespace* prefix[R] real[R] whitespace*
  // prefix[R]:         r[R] exactness | exactness r[R]

  // real[R]:           sign ureal[R]
  // ureal[R]:          uint[R] | decimal[R]
  // uint[R]:           d[R]+ #*
  // decimal[10]:       d[10]+                suffix
  //                  |           . d[10]+ #* suffix
  //                  | d[10+     . d[10]* #* suffix
  //                  | d[10]+ #+ .        #* suffix
  // suffix:            '' | exp-marker sign d[10]+
  // exp-marker:        e | s | f | d | l

  // sign:              '' | '+' | -
  // exactness:         '' | # i | # e
  // r[2]:              # b
  // r[8]:              # o
  // r[10]:             '' | # d
  // r[16]:             # x
  // d[R]:              <digit of radix R>

  const char *s = string_cstr(str);
  int n = string_length(str);
  char buffer[UNPARSED_FLONUM_SIZE + 1], *buf = buffer;
  int i = 0;                    // index of next char in str
  char c;                       // current char in str

  Flag is_exact = true;
  Flag expect_exact = true;
  Flag saw_exactness_prefix = false;

  // I assume the contents of buffer is never shorter than str...
  if (sizeof buffer < n + 2)
    fatal_error("Buffer overrun");

  // First we scan over the string collecting info on exactness, radix,
  // etc., and transcribing it into buffer in a format that strtod or
  // strtol can understand, and with leading and trailing whitespace trimmed.

  while (i < n && isspace(s[i]))
    ++i;
  if (i == n) return obj_false;
  c = s[i++];

  {
    Flag saw_radix_prefix = false;
    while (c == '#') {
      if (i == n) return obj_false;

      c = tolower(s[i++]);
      switch (c) {
      case 'i':
      case 'e':
        if (saw_exactness_prefix) return obj_false;
        saw_exactness_prefix = true;
        expect_exact = (c == 'e');
        break;
      case 'b':
      case 'o':
      case 'd':
      case 'x':
        if (saw_radix_prefix) return obj_false;
        saw_radix_prefix = true;
        radix = (c == 'b' ? 2 : c == 'o' ? 8 : c == 'd' ? 10 : 16);
        break;
      default:
        return obj_false;
      }
      if (i == n) return obj_false;
      c = s[i++];
    }
  }

  if (c == '+' || c == '-') {
    if (c == '-')
      *buf++ = c;
    if (i == n) return obj_false;
    c = s[i++];
  }
  
  if (radix != 10) {  // integers only, possibly inexact
    if (convert_digit(c, radix) == -1)
      return obj_false;
    *buf++ = c;
    while (i < n) {
      c = s[i++];
      if (convert_digit(c, radix) != -1)
        *buf++ = c;
      else if (c == '#') {
        is_exact = false;
        *buf++ = '0';
        while (i < n) {
          c = s[i++];
          if (c != '#') {
            --i;
            break;
          }
          *buf++ = '0';
        }
        break;
      } 
      else {
        --i;
        break;
      }
    }
  } 
  else {              // radix = 10, floats allowed
    // uint[R]:                d[R]+ #*
    // decimal[10]:            d[10]+                suffix
    // |           . d[10]+ #* suffix
    // | d[10+     . d[10]* #* suffix
    // | d[10]+ #+ .        #* suffix
    Flag digits_allowed = true, digits_required = true;
    while (isdigit(c)) {
      digits_required = false;
      *buf++ = c;
      if (i == n) goto scan_real_done;
      c = s[i++];
    }
    if (!digits_required) 
      while (c == '#') {
        is_exact = false;
        digits_allowed = false;
        *buf++ = '0';
        if (i == n) goto scan_real_done;
        c = s[i++];
      }
    if (c == '.') {
      is_exact = false;
      *buf++ = c;
      if (i == n) {
        if (digits_required)
          return obj_false;
        goto scan_real_done;
      }
      c = s[i++];
      if (digits_required && !isdigit(c))
        return obj_false;
      if (digits_allowed) {
        while (isdigit(c)) {
          *buf++ = c;
          if (i == n) goto scan_real_done;
          c = s[i++];
        }
      }
      while (c == '#') {
        if (i == n) goto scan_real_done;
        c = s[i++];
      }
    }
    else if (digits_required)
      return obj_false;

    // suffix:       '' | exp-marker sign d[10]+
    // exp-marker:   e | s | f | d | l
    if (strchr("esfdlESFDL", c)) {    // TODO also check radix==10, I guess
      is_exact = false;
      *buf++ = 'e';
      if (i == n) return obj_false;
      c = s[i++];
      if (c == '+' || c == '-') {
        if (c == '-') 
          *buf++ = c;
        if (i == n) return obj_false;
        c = s[i++];
      }
      if (!isdigit(c))
        return obj_false;
      *buf++ = c;
      while (i < n) {
        c = s[i++];
        if (!isdigit(c))
          break;
        *buf++ = c;
      }
    }
    else
      --i;
  }

scan_real_done:
  // At this point i = index of first char not in number.
  // Skip trailing spaces, and terminate buf.
  while (i < n && isspace(s[i]))
    ++i;
  if (i != n) return obj_false;
  *buf = '\0';

  // We've transcribed the number into buf in a format the C library should
  // understand; now let's convert it. (This won't be R4RS-compliant re
  // floating-point precision unless this system's strtod() is.)

  // Need to handle these cases:
  //  (column names: is_e = is_exact, xp = exactness prefix)
  //  where xp: "":   saw_exactness_prefix = false, expect_exact = false
  //        xp: "#e": saw_exactness_prefix = true,  expect_exact = true
  //        xp: "#i": saw_exactness_prefix = false, expect_exact = false

  //   str    | is_e | radix | xp | what to do
  //   42     | yes  |    10 |    | use strtoll, backstop with strtod on overflow
  //   #e42   | yes  |    10 | #e | use strtoll
  //   #x42   | yes  |  !=10 |    | use strtoll
  //   #x#e42 | yes  |  !=10 | #e | use strtoll
  //   #x#i42 | yes  |  !=10 | #i | use strtoll, coerce to flonum

  //   #x42.0 | no   |  !=10 |    | #f
  //   #x#e1.0| no   |  !=10 | #e | #f
  //   #x#i42.| no   |  !=10 | #i | #f

  //   #e42.0 | no   |    10 | #e | use strtod, coerce to fixnum
  //   42.0   | no   |    10 |    | use strtod
  //   #i42   | ?    |    10 | #i | use strtod

  if (is_exact) {
    char *end;
    long long ival = (errno = 0, strtoll(buffer, &end, radix));
    if (errno == 0 && *end == '\0') {
      if (expect_exact && int_is_fixnum(ival)) // xp is either "" or "#e"
        return make_fixnum(ival);
      else
        return make_flonum(ival);  // case like #i42 or <largedecimal>
    }
    // else fall through: e.g. <verylargedecimal>
  }

  if (radix != 10)
    return obj_false;

  char *end;
  double d = (errno = 0, strtod(buffer, &end));
  if (errno != 0 || *end != '\0')
    return obj_false;
  if (saw_exactness_prefix && expect_exact) { // case like #e<decimalfloat>
    // Same conversion logic as the inexact->exact primitive: (TODO unify the code?)
    if (d == floor(d) && FIXNUM_MIN <= d && d < (FIXNUM_MAX+1))
      return make_fixnum((Fixnum) d);
    return obj_false;
  }
  return make_flonum(d);
}

// Pre: buffer is big enough to hold any n converted in radix,
//      and 2 <= radix <= 36.
static void
unparse_int(char *buffer, Fixnum n, unsigned radix) {
  uint64_t u = (uint64_t)n;
  if (n < 0) {
    u = -u;
    *buffer++ = '-';
  }

  char stack[65], *end = stack + sizeof stack, *sp = end;

  // First compute the digits in reverse order:
  if (u == 0)
    *--sp = '0';
  else
    for (; u != 0; u /= radix) {
      unsigned digit = u % radix;
      *--sp = digit + (digit < 10 ? '0' : 'A' - 10);
    }

  // Then copy to the output:
  memcpy(buffer, sp, end - sp);
  buffer[end - sp] = '\0';
}

// This is not R4RS-compliant:
static void 
unparse_flonum(char *buf, Object num) {
  sprintf(buf, "%.16g", flonum_value(num));
  // Add a decimal point if necessary
  char *b = (*buf == '-' ? buf+1 : buf);
  size_t off = strspn(b, "0123456789");
  if ('\0' == b[off]) {
    b[off] = '.';
    b[off+1] = '\0';
  }
}

// RECOVERABLE
static Object 
number_to_string(Object num, Fixnum radix) {
  char buf[UNPARSED_FLONUM_SIZE + 1];
  assert(is_number(num));

  if (is_fixnum(num)) {
    if (!(2 <= radix && radix <= 36))
      vm_error("Unsupported radix", make_fixnum(radix));
    unparse_int(buf, fixnum_value(num), radix);
  } 
  else if (is_flonum(num)) {
    if (radix != 10)
      vm_error("Unsupported radix", make_fixnum(radix));
    unparse_flonum(buf, num);
  } 
  else
    type_error(num);

  return c_string(buf);
}

// RECOVERABLE
static Object
read_atom(FILE *in, int c) {
  char buf[1024];
  char *b = buf;
  *b++ = tolower(c);

  for (;;) {
    c = getc(in);
    switch (c) {

    case EOF:
      if (ferror(in)) {
        if (errno == EINTR)       // boy is this annoying
          break;
        io_error(errno);
      }
      goto done;

    case '(': case ')': case ';': 
    case '"': case '`': case ',': case '@':
      ungetc(c, in);
      goto done;

    default:
      if (isspace(c)) {
        ungetc(c, in);
        goto done;
      }
      if (buf + sizeof buf - 1 <= b)
        fatal_error("Buffer overflow"); // FIXME: hard limit
      *b++ = tolower(c);
    }
  }

done:
  *b = '\0';
  Object s = c_string(buf); // TODO define string_to_number in terms
                            // of a c_string_to_number not on the heap
  Object n = string_to_number(s, 10);
  return is_true(n) ? n : string_to_symbol(s);
}


// Numeric operations

// RECOVERABLE
static double
as_double(Object n) {
  if (is_flonum(n))
    return flonum_value(n);
  if (!is_fixnum(n))
    type_error(n);
  return fixnum_value(n);
}

// RECOVERABLE
static int
as_int(Object n) {
  if (is_flonum(n)) {
    double d = flonum_value(n);
    // Check range before casting to avoid UB
    if (d < INT_MIN || d > INT_MAX || d != floor(d))
      vm_error("Not an integer", n);
    return (int) d;
  }
  if (!is_fixnum(n))
    type_error(n);
  return fixnum_value(n);
}

// RECOVERABLE
static Object
float_op1(double (*oper)(double), Object n) {
  return make_flonum(oper(as_double(n)));
}

static void
division_by_zero(void) {
  vm_error("Division by 0", nil);
}

// If n and d are not both integers, or d is 0, raise an error.
// Else set *quot and *rem to the result of dividing n by d.
// Rounding is towards 0.
// RECOVERABLE
static void 
divide_inexact(double *quot, double *rem, double n, double d) {
  if (d == 0)           // what about 0 % 0?
    division_by_zero();
  double foo;
  if (0 != modf(d, &foo))
    vm_error("Bad type", make_flonum(d));
  if (0 != modf(n, &foo))
    vm_error("Bad type", make_flonum(n));
  *rem = d * modf(n / d, quot);
}

// RECOVERABLE
static Object 
modulo(Object n, Object d) {
  if (is_fixnum(n) && is_fixnum(d)) {
    if (fixnum_value(d) == 0)
      division_by_zero();
    Fixnum dv = fixnum_value(d);
    Fixnum r = REMAINDER(fixnum_value(n), dv);
    if (dv < 0 ? 0 < r : r < 0)
      r += dv;
    return int_is_fixnum(r) ? make_fixnum(r) : make_flonum(r);
  }
  else {
    double q, r, dv = as_double(d);
    divide_inexact(&q, &r, as_double(n), dv);
    if (dv < 0 ? 0 < r : r < 0)
      r += dv;
    return make_flonum(r);
  }
}

// RECOVERABLE
static Object 
my_remainder(Object n, Object d) {
  if (is_fixnum(n) && is_fixnum(d)) {
    if (fixnum_value(d) == 0)
      division_by_zero();
    Fixnum r = REMAINDER(fixnum_value(n), fixnum_value(d));
    return int_is_fixnum(r) ? make_fixnum(r) : make_flonum(r);
  }
  else {
    double q, r;
    divide_inexact(&q, &r, as_double(n), as_double(d));
    return make_flonum(r);
  }
}

// RECOVERABLE
static Object 
quotient(Object n, Object d) {
  if (is_fixnum(n) && is_fixnum(d)) {
    if (fixnum_value(d) == 0)
      division_by_zero();
    Fixnum r = QUOTIENT(fixnum_value(n), fixnum_value(d));
    return int_is_fixnum(r) ? make_fixnum(r) : make_flonum(r);
  }
  else {
    double q, r;
    divide_inexact(&q, &r, as_double(n), as_double(d));
    return make_flonum(q);
  }
}

// RECOVERABLE
static Object
multiply(Object n1, Object n2) {
  if (is_fixnum(n1) && is_fixnum(n2)) {
    Fixnum i1 = fixnum_value(n1), i2 = fixnum_value(n2);
    __int128 product = (__int128) i1 * (__int128) i2;
    if (product >= FIXNUM_MIN && product <= FIXNUM_MAX)
      return make_fixnum((Fixnum) product);
    else
      return make_flonum((double) product);
  } else
    return make_flonum(as_double(n1) * as_double(n2));
}

// RECOVERABLE
static Object 
divide(Object n1, Object n2) {
  if (is_fixnum(n1) && is_fixnum(n2)) {
    Fixnum i1 = fixnum_value(n1), i2 = fixnum_value(n2);
    if (i2 == 0)
      division_by_zero();
    return i1 % i2 == 0 ? make_fixnum(i1 / i2) 
                        : make_flonum((double) i1 / i2);
  }
  else {
    double d2 = as_double(n2);
    if (d2 == 0)
      division_by_zero();
    return make_flonum(as_double(n1) / d2);
  }
}


// --- Writing and displaying ---

static void 
write_string(FILE *file, Object str) {
  const char *s = string_cstr(str);
  int i, l = string_length(str);
  
  put_char('"', file);
  for (i = 0; i < l; ++i) {
    char c = s[i];
    if (c == '"' || c == '\\')
      put_char('\\', file);
    if (isprint(c))
      put_char(c, file);
    else
      if (fprintf(file, "\\%03o", c) < 0)
        io_error(errno);
  }
  put_char('"', file);
}

static void 
put_object(FILE *file, Object obj, Flag displaying) {
  if (!is_boxed(obj)) {
      if (is_fixnum(obj)) {
        if (fprintf(file, "%lld", (long long) fixnum_value(obj)) < 0)
          io_error(errno);
      }
      else if (is_null(obj))
        put_string("()", file);
      else if (is_boolean(obj))
        put_string(is_true(obj) ? "#t" : "#f", file);
      else if (is_eof_object(obj))
        put_string("#!eof", file);
      else if (is_unbound(obj))
        put_string("#!unbound", file);
      else if (is_char(obj)) {
        char c = char_value(obj);
        if (displaying)
          put_char(c, file);
        else {
          put_string("#\\", file);
          if (isgraph(c))
            put_char(c, file);
          else {
            const char *name;
            switch (c) {
            case ' ':  name = "space";   goto named;
            case '\t': name = "tab";     goto named;
            case '\n': name = "newline"; goto named;
            case '\r': name = "return";  goto named;
            named:
              put_string(name, file);
              break;
            default: 
              // FIXME: get reader to understand this
              if (fprintf(file, "\\%03o", c & 0xff) < 0)
                io_error(errno);
              break;
            }
          }
        }
      } 
      else
        unreachable();
  } 
  else {                        // is_boxed(obj)
    switch (object_tag(obj)) {
    case a_flonum: {
      char buf[UNPARSED_FLONUM_SIZE + 1];
      unparse_flonum(buf, obj);
      put_string(buf, file);
      break;
    }
    case a_string:
      if (displaying)
        display_string(obj, file);
      else
        write_string(file, obj);
      break;
    case a_symbol: 
      // FIXME: backslashify if necessary
      display_string(symbol_to_string(obj), file);
      break;
    case an_input_port:
      put_string("#<input port>", file);
      break;
    case an_output_port:
      put_string("#<output port>", file);
      break;
    case a_closure: {
      Object code = closure_code(obj);
      Object name = nil;
      if (is_vector(code) && vector_length(code) == 6) // TODO obscure
        name = vector_ref(code, 3);
      put_string("#<procedure", file);
      put_char(' ', file);
      for (; is_pair(name); name = cdr(name)) {
        // display a nested procedure name like #<procedure inner,outer>
        put_object(file, car(name), false);
        put_char(',', file);
      }
      put_object(file, name, false);
      put_char('>', file);
      break;
    }
    case a_pair:
      put_char('(', file);
      put_object(file, car(obj), displaying);
      for (;;) {
        obj = cdr(obj);
        if (!is_pair(obj)) break;
        put_char(' ', file);
        put_object(file, car(obj), displaying);
      } 
      if (obj != nil) {
        put_string(" . ", file);
        put_object(file, obj, displaying);
      }
      put_char(')', file);
      break;
    case a_vector:
      put_string("#(", file);
      for (int i = 0; i < vector_length(obj); ++i) {
        if (i != 0)
          put_char(' ', file);
        put_object(file, vector_ref(obj, i), displaying);
      }
      put_char(')', file);
      break;
    default:
      unreachable();
    }
  }
}

static void 
write_object(FILE *file, Object obj) {
  put_object(file, obj, false);
}


// --- fasl reader ---

#include "init.h"

static const unsigned char the_fasl[] = {
#include "init.c"
};
static const int fasl_size = sizeof the_fasl;
static int fasl_ptr = 0;

static int
fasl_next(void) {
  assert(fasl_ptr < fasl_size);
  return the_fasl[fasl_ptr++];
}

static int 
read_unsigned8(void) {
  return fasl_next();
}

static int64_t
read_int(void) {
  // 7-bit varint with zigzag decoding
  uint64_t u = 0;
  int shift = 0;
  int byte;
  do {
    byte = read_unsigned8();
    u |= (uint64_t)(byte & 0x7f) << shift;
    shift += 7;
  } while (byte & 0x80);
  // zigzag decode: 0->0, 1->-1, 2->1, 3->-2, ...
  return (u >> 1) ^ -(int64_t)(u & 1);
}

static Object 
undump_string(void) {
  int i, n = read_int();
  Object str = make_string(n);
  unsigned char *s = string_ptr(str);
  for (i = 0; i < n; ++i)
    s[i] = read_unsigned8();
  return str;
}

#define PUSH(x) do {                   \
        if (FASL_STACK_SIZE <= sp)          \
          stack_error();               \
        stack_base[sp++] = (x);        \
                } while (0)

#define POP(x) do {                    \
        if (sp < 0)                    \
          stack_error();               \
        (x) = stack_base[--sp];        \
               } while (0)

static void
stack_error(void) {
  vm_error("Unbalanced stack in read-fasl", nil);
}

// RECOVERABLE
static Object 
read_fasl(void) {
  Object fasl_stack = make_vector(FASL_STACK_SIZE, nil);
  Object *stack_base = vector_ptr(fasl_stack);
  int sp = 0;                   // stack pointer

  Object o1 = nil, o2 = nil, o3 = nil;

  enum { max_seen = 5000 };       // TODO make a config param, I guess
  Object seen_vector = make_vector(max_seen, nil);
  int nseen = 0;

  fasl_ptr = 0;
  while (fasl_ptr < fasl_size) {
    int tag = fasl_next();
    switch (tag) {
      default:
        vm_error("Unrecognized tag", make_fixnum(tag));
      break;
      case ini_cons: 
        POP(o1);
        POP(o2);
        o3 = cons(o1, o2);
        if (nseen < max_seen) vector_set(seen_vector, nseen++, o3);
        PUSH(o3);
      break;
      case ini_ref: {
        int i = read_int();
        if (i < 0 || nseen <= i) vm_error("=ref out of range", make_fixnum(i));
        PUSH(vector_ref(seen_vector, i));
      }
      break;
      case ini_vector: {
        int i, n = read_int();
        Object vec = make_vector(n, nil);
        Object *v = vector_ptr(vec);
        for (i = 0; i < n; ++i)
          POP(v[i]);
        PUSH(vec);
      }
      break;
      case ini_closure:
        POP(o1);
        POP(o2);
        PUSH(make_closure(o1, o2));
      break;
      case ini_symbol: {
        int i, n = read_int();
        Object str = make_string(n);
        unsigned char *s = string_ptr(str);
        for (i = 0; i < n; ++i)
          s[i] = tolower(read_unsigned8());
        o3 = string_to_symbol(str);
        PUSH(o3);
        if (nseen < max_seen) vector_set(seen_vector, nseen++, o3);
      }
      break;
      case ini_nil:
        PUSH(nil);
      break;
      case ini_int:
        PUSH(make_fixnum(read_int()));
      break;
      case ini_true: 
        PUSH(obj_true);
      break;
      case ini_false: 
        PUSH(obj_false);
      break;
      case ini_string:
        PUSH(undump_string());
      break;
      case ini_char:
        PUSH(make_char((char) read_unsigned8()));
    }
  }
  if (sp == 1) {
    POP(o1);
    return o1;
  }
  vm_error("Premature EOF or stack size != 1", nil);
  return nil;
}


// callouts

static Object
integer_to_char(Object x0) {
  unsigned n = as_int(x0);
  if (256 <= n)
    range_error(n);
  return make_char(n);
}

static Object
close_input_port(Object x0) {
  check_type(is_input_port(x0), x0);
  close_port(x0);
  return void_marker;
}

static Object
close_output_port(Object x0) {
  check_type(is_output_port(x0), x0);
  close_port(x0);
  return void_marker;
}

static Object
open_input_file(Object x0) {
  check_type(is_string(x0), x0);
  return open_file(x0, an_input_port, "r");
}

static Object
open_output_file(Object x0) {
  check_type(is_string(x0), x0);
  return open_file(x0, an_output_port, "w");
}

static Object
prim_list_length(Object x0) {
  return make_fixnum(list_length(x0));
}

static Object
prim_skip_blanks(Object x0) {
  check_type(is_input_port(x0), x0);
  check_openness(x0);
  skip_blanks(port_file(x0));
  return void_marker;
}

static Object
prim_flush_input_line(Object x0) {
  check_type(is_input_port(x0), x0);
  check_openness(x0);
  flush_input_line(port_file(x0));
  return void_marker;
}

static Object
expt(Object x1, Object x0) {
  if (is_fixnum(x1) && is_fixnum(x0)) {
    double p = pow(fixnum_value(x1), fixnum_value(x0));
    // Try to return exact fixnum if possible. Must check:
    // 1. p is in int64 range (to avoid UB in cast)
    // 2. Conversion is exact (round-trip check)
    // 3. Result fits in fixnum range
    if (p >= (double)INT64_MIN && p < (double)INT64_MAX) {
      Fixnum i = (Fixnum) p;
      if ((double)i == p && int_is_fixnum(i))
        return make_fixnum(i);
    }
    return make_flonum(p);
  }
  else
    return make_flonum(pow(as_double(x1),
                           as_double(x0)));
}

static Object
prim_atan(Object x1, Object x0) {
  return make_flonum(atan2(as_double(x1),
                           as_double(x0))); 
}

static Object
prim_read_atom(Object x1, Object x0) {
  check_type(is_input_port(x1), x1);
  check_type(is_char(x0), x0);
  check_openness(x1);
  return read_atom(port_file(x1), char_value(x0));
}

static Object
prim_write(Object x1, Object x0) {
  check_type(is_output_port(x0), x0);
  check_openness(x0);
  FILE *fp = port_file(x0);
  write_object(fp, x1);
  if (0 != fflush(fp))
    io_error(errno);
  return void_marker;
}

static Object
prim_display(Object x1, Object x0) {
  check_type(is_output_port(x0), x0);
  check_openness(x0);
  FILE *fp = port_file(x0);
  put_object(fp, x1, true);
  if (0 != fflush(fp))
    io_error(errno);
  return void_marker;
}

static Object
prim_number_to_string(Object x1, Object x0) {
  check_type(is_fixnum(x0), x0);
  return number_to_string(x1, fixnum_value(x0));
}

static Object
prim_string_to_number(Object x1, Object x0) {
  check_type(is_string(x1), x1);
  check_type(is_natnum(x0), x0);
  return string_to_number(x1, fixnum_value(x0));
}


static Object error_symbol;
static Object all_entered_code_vectors_symbol;

static void 
setup(void) {
  symbol_table = make_vector(101, nil);
  void_marker = c_symbol("#!%void");

  current_input_port = make_port(an_input_port, stdin);
  current_output_port = make_port(an_output_port, stdout);

  global_renv = allot_vector(0);

  code_vector_symbol = c_symbol("code-vector");
  error_symbol = c_symbol("%error");

  all_entered_code_vectors_symbol = c_symbol("%all-entered-code-vectors");
  set_global_value(all_entered_code_vectors_symbol, nil);

  {
    Object b = make_string(1);
    string_ptr(b)[0] = bop_halt;
    halt_code = 
      make_code_vector(global_renv, b, c_symbol("halt_code"), nil);
  }

  {
    Object b = make_string(1);
    string_ptr(b)[0] = bop_invoke;
    just_invoke_code = 
      make_code_vector(global_renv, b, c_symbol("just_invoke_code"), nil);
  }

  {
    unsigned char bopcode[] = { bop_params, 1,
                                bop_varref, 1, 0,
                                bop_set_cc,
                                bop_varref, 0, 0,
                                bop_restore };
    Object str = make_string(sizeof bopcode);
    memcpy(string_ptr(str), bopcode, sizeof bopcode);
    Object locals_map =
      cons(cons(c_symbol("value"), nil),
            cons(cons(c_symbol("stack-vector"), nil),
                  nil));
    reified_cont_code = 
      make_code_vector(global_renv, str, c_symbol("reified_cont_code"),
                        locals_map);
  }    
}

fast Object *
lookup_renv(Object renv, unsigned frame, unsigned offset) {
  for (; 0 < frame; --frame)
    renv = vector_ref_unsafe(renv, 0);
  assert(offset + 1 < vector_length(renv));
  return vector_ptr(renv) + (offset + 1);
}

static void
unexpected_vm_error(void) {
  fprintf(stderr, "[Error with no handler] %s\n", vm_error_message);
  write_object(stderr, vm_error_irritant);
  putc('\n', stderr);
  exit(1);
}


typedef struct Interpreter {
  Object stack_vec;
  Object code, renv;
  int pc, stack_ptr, frame_ptr;
} Interpreter;


#define stack_limit     VM_STACK_SIZE
#define top()           (stack[stack_ptr - 1])
#define set_top(x)      (stack[stack_ptr - 1] = (x))
#define pop()           (stack[--stack_ptr])
#define drop()          (--stack_ptr)
#define push(x)         (stack[stack_ptr++] = (x))

#define need(n)         do {                                    \
                          if (stack_limit <= stack_ptr + (n))   \
                            goto stack_overflow;                \
                        } while (0)

// need error checks on these...

#define get_byte()      (bvec[pc++])
#define get_short()     (pc += 2, (bvec[pc-2]) * 256 + (bvec[pc-1]))
#define get_datum()     (constants[get_byte()])

#define save_state(scode, senv, soffset)       \
  do {                                         \
    Object code_ = (scode);                    \
    Object renv_ = (senv);                     \
    int offset_ = (soffset);                   \
    need(4);                                   \
    push(make_fixnum(offset_));                \
    push(code_);                               \
    push(renv_);                               \
    push(make_fixnum(frame_ptr));              \
    frame_ptr = stack_ptr;                     \
  } while (0)

#define restore_state()                          \
  do {                                           \
    assert(stack_ptr == frame_ptr);              \
    frame_ptr = fixnum_value(pop());             \
    assert(is_vector(top()));                    \
    renv = pop();                                \
    assert(is_vector(top()));                    \
    code = pop();                                \
    bvec = string_ptr(vector_ref_unsafe(code, 2));      \
    constants = vector_ptr(vector_ref_unsafe(code, 1)); \
    pc = fixnum_value(pop());                    \
  } while (0)

#define flush_registers()                       \
  do {                                          \
    interp->code      = code;                   \
    interp->renv      = renv;                   \
    interp->pc        = pc;                     \
    interp->stack_ptr = stack_ptr;              \
    interp->frame_ptr = frame_ptr;              \
  } while (0)

typedef Object (*callout1_t) (Object);
typedef Object (*callout2_t) (Object, Object);

#ifdef BYTEOP_PROFILING
enum { nbops = bop_halt + 1 };
static unsigned byteop_count[nbops][nbops];
#endif

#ifdef STACK_DEPTH_PROFILING
static unsigned stack_depth_count[1024];
#endif

// Pre: CODE is a code_vector that ends with a restore instruction.
static Object
enter_interpreter(Interpreter *interp) {
  Object acc = obj_false;
  const char *error_msg = "";

  // We initialize this once and for all because it won't change.
  Object *stack = vector_ptr(interp->stack_vec);

  // Hopefully these will be kept in registers
  Object code, renv;
  unsigned pc, stack_ptr, frame_ptr;
  Object *constants;
  const unsigned char *bvec;
  
  unsigned char byteop = bop_halt;

#ifdef BYTEOP_PROFILING
  unsigned char previous_byteop;
#endif

  code      = interp->code;
  renv      = interp->renv;
  pc        = interp->pc;
  stack_ptr = interp->stack_ptr;
  frame_ptr = interp->frame_ptr;
  constants = vector_ptr(vector_ref(code, 1));
  bvec      = string_ptr(vector_ref(code, 2));

  // The bytecode execution loop
  for (;;) {
    assert(pc < string_length(vector_ref(code, 2)));
    assert(frame_ptr <= stack_ptr && stack_ptr < stack_limit);

#ifdef BYTEOP_PROFILING
    previous_byteop = byteop;
#endif

    byteop = get_byte();

#ifdef BYTEOP_PROFILING
    byteop_count[previous_byteop][byteop] += 1;
#endif

#ifdef STACK_DEPTH_PROFILING
    stack_depth_count[stack_ptr - frame_ptr] += 1;
#endif

    switch (byteop) {

    default: 
      error_msg = "Bad opcode";
      acc = make_fixnum(byteop);
      goto vm_error_label;

    unbound_error_label:
      error_msg = "Unbound variable";
      goto vm_error_label;

    type_error_label:
      error_msg = "Bad type";
      goto vm_error_label;

    range_error_label:
      error_msg = "Argument out of range";
      goto vm_error_label;

    io_error_label:
      error_msg = strerror(errno);
      goto vm_error_label;

    closed_port_error_label:
      error_msg = "Access to closed port";
      goto vm_error_label;

    vm_error_label:
      vm_error_message = error_msg;
      vm_error_irritant = acc;
      flush_registers();
      signal_vm_error();
      break;

    stack_overflow: 
      fatal_error("Stack overflow");

#define vm_type_error(x)    do { acc = (x); goto type_error_label; } while (0)
#define vm_range_error(x)  \
               do { acc = make_fixnum(x); goto range_error_label; } while (0)

#define vm_check_type(f,x)  do { if (!(f)) vm_type_error(x); } while (0)

#include "prims.h"
#include "byteops.c"

    }
  }
}

static Object the_stack = nil;

// This is like save_state(code, renv, pc) inside enter_interpreter
static void
push_frame(Interpreter *i, Object code, Object renv, unsigned pc) {
  Object *s = vector_ptr(i->stack_vec) + i->stack_ptr;
  if (stack_limit <= i->stack_ptr + 4)
    fatal_error("Stack overflow");

  s[0] = make_fixnum(pc);
  s[1] = code;
  s[2] = renv;
  s[3] = make_fixnum(i->frame_ptr);
  i->stack_ptr += 4;
  i->frame_ptr = i->stack_ptr;
}

static Object
interpret(Object code, Object renv) {
  // volatile needed for setjmp
  volatile Interpreter i;

  if (!is_vector(the_stack))
    the_stack = make_vector(VM_STACK_SIZE, obj_false);

  i.stack_vec = the_stack;
  i.code      = code;
  i.renv      = renv;
  i.pc        = 0;
  i.stack_ptr = 0;
  i.frame_ptr = 0;

  push_frame((Interpreter*)&i, halt_code, global_renv, 0);

  if (0 == setjmp(vm_error_catch_point)) 
    return enter_interpreter((Interpreter*)&i);

  {
    // The interpreter raised an error, let's handle it:
    // try to push a frame and invoke the ERROR procedure.

#ifndef NDEBUG
  printf("\n[VM Error!] %s\n", vm_error_message);
  write_object(stdout, vm_error_irritant);
  printf("\n");
#endif

    // Make sure we have a clean stack frame...
    if (i.pc < string_length(vector_ref(i.code, 2)))
      push_frame((Interpreter*)&i, i.code, i.renv, i.pc);
    else
      i.stack_ptr = i.frame_ptr;
  
    // ...and try to invoke the ERROR procedure.
    Object error_proc = global_value(error_symbol);
    if (!is_closure(error_proc))
      unexpected_vm_error();

    // Push the arguments
    Object *s = vector_ptr(i.stack_vec) + i.stack_ptr;
    if (stack_limit <= i.stack_ptr + 3)
      fatal_error("Stack overflow");
    s[0] = c_string(vm_error_message);
    s[1] = vm_error_irritant;
    s[2] = error_proc;
    i.stack_ptr += 3;

    // And go.
    i.code = just_invoke_code;
    i.pc = 0;
    return enter_interpreter((Interpreter*)&i);
  }
}

// Call closure with no parameters.
static Object
invoke0(Object closure) {
  Object renv = closure_renv(closure);
  Object extended_env = make_vector(1, renv);
  return interpret(closure_code(closure), extended_env);
}


static void
run_fasl(void) {
  volatile Object codes = read_fasl();
  check_type(is_vector(codes), codes);

  for (volatile Fixnum i = 0; i < vector_length(codes); ++i) {
    Object o = vector_ref(codes, i);
    interpret(o, global_renv);
    // Reestablish catcher clobbered by interpret() FIXME
    if (0 != setjmp(vm_error_catch_point))
      unexpected_vm_error();
  }
}

// Return a list of argv[1]..argv[argc-1]
static Object
command_line_arglist(int argc, char **argv) {
  Object arglist = nil;
  for (int i = argc - 1; 0 <= i; --i)
    arglist = cons(c_string(argv[i]), arglist);
  return arglist;
}

int 
main(int argc, char **argv) {
  clock_start = clock();

#ifndef NDEBUG
  setvbuf(stdout, NULL, _IONBF, 0);
#endif

  //  This tweak doesn't seem to be worth the space cost (see gc.h):
  //  GC_free_space_divisor = 2; 
  //  Nor this (slower on bench):
  //  GC_enable_incremental();

  if (0 != setjmp(vm_error_catch_point))
    unexpected_vm_error();
  setup();

  set_global_value(c_symbol("%command-line-arguments"),
                    command_line_arglist(argc, argv));

  run_fasl();

  // Reestablish catcher clobbered by run_fasl()
  if (0 != setjmp(vm_error_catch_point))
    unexpected_vm_error();

  { // Start the main loop if the fasl set one up
    Object driver = c_symbol("%start-scheming");
    Object proc = global_value(driver);
    if (is_closure(proc))
      invoke0(proc);
  }

#ifdef BYTEOP_PROFILING
  {
    FILE *out = fopen("byteop_profile", "w");
    if (!out)
      fatal_error(strerror(errno));

    for (int i = 0; i < nbops; ++i)
      for (int j = 0; j < nbops; ++j)
        if (byteop_count[i][j] != 0)
          fprintf(out, "%2d %2d %10u\n", i, j, byteop_count[i][j]);

    fclose(out);
  }
#endif

#ifdef STACK_DEPTH_PROFILING
  {
    FILE *out = fopen("stack_depth_profile", "w");
    if (!out)
      fatal_error(strerror(errno));

    for (int i = 0; i < 1024; ++i)
      if (stack_depth_count[i] != 0)
        fprintf(out, "%4d %10u\n", i, stack_depth_count[i]);

    fclose(out);
  }
#endif
      
  return 0;
}
