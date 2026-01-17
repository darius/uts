/* Parameters */

#define VM_STACK_SIZE 	16384
#define FASL_STACK_SIZE 1000


/* Implementation dependent things */

#define int32 		int
#define unsigned32 	unsigned int
#define int64		long long int

#define highbit		(1 << 31)

#define ashr2(i)	((i) >> 2)

#define QUOTIENT(n,d)	((n) / (d))
#define REMAINDER(n,d)	((n) % (d))

#define UNPARSED_FLONUM_SIZE 1024

#define fast 		static inline

/* maybe something about signed vs. unsigned chars here? */
