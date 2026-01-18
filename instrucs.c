/* to be processed then #included in uts.c */
/* scheme names for ops are mangled... */
/* is get_byte signed, or what? */


lit:				/* d */
  { 
    need (1);
    push (get_datum ()); 
  }
  
varref:				/* c c */
  { 
    unsigned frame = get_byte ();
    unsigned offset = get_byte ();
#ifdef BYTEOP_PROFILING
    if (frame == 0)
      {				/* break out stats for the innermost frame */
	byteop_count [previous_byteop][byteop] -= 1;
	byteop = 23;
	byteop_count [previous_byteop][byteop] += 1;
      }
#endif
    need (1);
    push (*lookup_lex_env (lex_env, frame, offset));
  }

varset:				/* c c */
  {
    unsigned frame = get_byte ();
    unsigned offset = get_byte ();
    *lookup_lex_env (lex_env, frame, offset) = top ();
  }

global_ref:			/* d */
  {
    Object var = get_datum ();
    Object value = global_value (var);
    if (is_unbound (value)) 
      {
	acc = var;
	goto unbound_error_label;
      }
    need (1);
    push (global_value (var));
  }

global_set:			/* d */
  {
    Object var = get_datum ();
    Object value = global_value (var);
    if (is_unbound (value)) 
      {
	drop ();
	acc = var;
	goto unbound_error_label;
      }
    set_global_value (var, top ());
  }

global_define:			/* d */
  {
    Object var = get_datum ();
    set_global_value(var, top ());
    set_top (var);
  }

branch:				/* c c */
  {
    if (is_true (pop ()))
      pc += 2;
    else 
      {
	unsigned offset = get_short ();
	pc += offset;
      }
  }

jump:				/* c c */
  {
    unsigned offset = get_short ();
    pc += offset;
  }

proc:				/* d */
  {
    need (1);
    push (make_closure (lex_env, get_datum ()));
  }

extend_normal_env:		/* c */
  {
    unsigned num_formals = get_byte ();

    if (stack_ptr - frame_ptr != num_formals) /* assumes stack grows upward */
      { 
	/* need to be able to have > 1 errobj:
	   fprintf (stderr, "wna %d, should be %d\n", 
	            stack_ptr - frame_ptr, num_formals);
         */
	int num_args = stack_ptr - frame_ptr;
	stack_ptr = frame_ptr;
	restore_state ();
	acc = make_fixnum (num_args);
	error_msg = "Wrong number of arguments";
	goto vm_error_label;
      } 
    else 
      {
	unsigned i;
	Object new_lex_env = allot_vector (1 + num_formals);
	vector_set (new_lex_env, 0, lex_env);
	for (i = num_formals; i != 0; --i)
	  vector_set (new_lex_env, i, pop ());
	lex_env = new_lex_env;
      }
  }

extend_rest_env:		/* c */
  {
    unsigned min_num_args = get_byte ();
    unsigned actuals = stack_ptr - frame_ptr;
    if (actuals < min_num_args) 
      {
      /*fprintf (stderr, "wna %d, should be >= %d\n", actuals, min_num_args); */
	int num_args = stack_ptr - frame_ptr;
	stack_ptr = frame_ptr;
	restore_state ();
	acc = make_fixnum (num_args);
	error_msg = "Wrong number of arguments";
	goto vm_error_label;
      } 
    else 
      {
	Object new_lex_env = allot_vector (1 + 1 + min_num_args);
	vector_set (new_lex_env, 0, lex_env);
	{
	  unsigned i;
	  Object rest_args = nil;
	  for (i = actuals - min_num_args; i != 0; --i)
	    rest_args = cons (pop (), rest_args);
	  vector_set (new_lex_env, 1 + min_num_args, rest_args);
	  for (i = min_num_args; i != 0; --i)
	    vector_set (new_lex_env, i, pop ());
	  lex_env = new_lex_env;
	}
      }
  }

restore:			/*  */
  {
    Object result = pop ();
    restore_state ();
    push (result);
  }

#define vm_type_error(x)    do { acc = (x); goto type_error_label; } while (0)
#define vm_range_error(x)  \
               do { acc = make_fixnum (x); goto range_error_label; } while (0)

#define vm_check_type(f,x)  do { if (!(f)) vm_type_error (x); } while (0)

invoke:				/*  */
  {
  apply_proc:
    if (!is_closure (top ())) 
      {
	acc = top ();
	stack_ptr = frame_ptr;
	goto type_error_label;
      }
    pc = 0;
    code = closure_code (top ());
    lex_env = closure_lex_env (pop ());
    constants = vector_ptr (vector_ref (code, 1));
    bvec = string_ptr (vector_ref (code, 2));
#ifdef FUNCTION_PROFILING
    {
      Object c = vector_ref (code, 4);
      if (is_fixnum (c))
	{
	  int count = fixnum_value (c) + 1;
	  if (int_is_fixnum (count))
	    vector_set (code, 4, make_fixnum (count));
	  if (count == 2)	/* don't link in until 2nd hit */
	    set_global_value (
	      all_entered_code_vectors_symbol,
	      cons (code, global_value (all_entered_code_vectors_symbol)));
	}
    }
#endif
  }

save:				/* c c */
  {
    unsigned offset = get_short ();
    save_state (code, lex_env, pc + offset);
  }

apply:				/*  */
  {
    int num_args = stack_ptr - frame_ptr;  /* assumes stack grows upward */
    if (num_args < 2) 
      { 
	stack_ptr = frame_ptr;
	restore_state ();
	acc = make_fixnum (num_args);
	error_msg = "Wrong number of arguments";
	goto vm_error_label;
      } 
    else
      {
	Object list = top (), rest = list;
	Object proc = stack [frame_ptr];
	int i;
	for (i = frame_ptr; i < stack_ptr - 2; ++i)
	  stack [i] = stack [i+1];
	stack_ptr = i;
	
	for (; is_pair (rest); rest = cdr (rest)) 
	  {
	    need (2);		/* this accounts for the following push(proc) */
	    push (car (rest));
	  }
	push (proc);
	
	if (!is_null (rest)) 
	  {
	    stack_ptr = frame_ptr;
	    restore_state ();
	    vm_type_error (list);
	  }
	goto apply_proc;
      }
  }

get_cc:				/*  */
  {
    int i;
    Object saved_stack, env;

    /* Make saved_stack hold a copy of the stack up to the current cont: */
    saved_stack = allot_vector (frame_ptr);
    for (i = frame_ptr - 1; 0 <= i; --i)
      vector_set (saved_stack, i, stack [i]);

    /* Now give env an env frame: */
    env = allot_vector (2);
    vector_set (env, 0, global_lex_env);
    vector_set (env, 1, saved_stack);

    /* Create a closure for the reified cont: */
    need (1);
    push (make_closure (env, reified_cont_code));
  }

set_cc:				/*  */
  {
    /* At this point top() points to a vector holding the complete
       stack contents of the cont.  The SET-CC instruction is used
       only in a situation where the current stack frame is empty,
       so we don't have to append that frame to the restored cont. */
    int i, limit;
    Object saved_stack = top ();

    assert (stack_ptr == frame_ptr + 1);

    if (!is_vector (saved_stack)) 
      {
	drop ();
	vm_type_error (saved_stack);
      }

    limit = vector_length (saved_stack);
    if (stack_limit <= limit)
      fatal_error ("Stack in continuation is too big to restore");
    for (i = limit - 1; 0 <= i; --i)
      stack [i] = vector_ref (saved_stack, i);
    stack_ptr = frame_ptr = limit;
  }


p0_current_input_port:	{ acc = current_input_port; }
p0_current_output_port:	{ acc = current_output_port; }
p0_runtime: 		{ acc = make_flonum ((clock () - clock_start) / 
					     (double) CLOCKS_PER_SEC); }

#define callout1(c)   do { c_routine = (c); goto callout1_label; } while (0)
#define callout2(c)   do { c_routine = (c); goto callout2_label; } while (0)

p1_booleanP:	{ acc = make_boolean (is_boolean (x0)); }
p1_charP:	{ acc = make_boolean (is_char (x0)); }
p1_eof_objectP: { acc = make_boolean (is_eof_object (x0)); }
p1_exactP:	{ acc = make_boolean (is_fixnum (x0)); }
p1_inexactP:	{ acc = make_boolean (is_flonum (x0)); }
p1_input_portP: { acc = make_boolean (is_input_port (x0)); }
p1_integerP:	{ acc = make_boolean (is_fixnum (x0)
				      || (is_flonum (x0)
					  && flonum_value (x0) == 
					     floor (flonum_value (x0)))); }
p1_numberP:	{ acc = make_boolean (is_number (x0)); }
p1_output_portP:{ acc = make_boolean (is_output_port (x0));}
p1_pairP:	{ acc = make_boolean (is_pair (x0)); }
p1_procedureP:  { acc = make_boolean (is_closure (x0)); }
p1_stringP:	{ acc = make_boolean (is_string (x0)); }
p1_symbolP:	{ acc = make_boolean (is_symbol (x0)); }
p1_vectorP:	{ acc = make_boolean (is_vector (x0)); }
p1_floor:	{ if (is_flonum (x0))
		    acc = make_flonum (floor (flonum_value (x0)));
		  else if (is_fixnum (x0))
		    acc = x0;
                  else 
		    vm_type_error (x0); }
p1_round: 	{ if (is_flonum (x0))
                    acc = make_flonum (my_round (flonum_value (x0)));
                  else if (is_fixnum (x0))
                    acc = x0;
		  else
		    vm_type_error (x0); }
p1_exactTOinexact:
		{ if (is_flonum (x0))
		    acc = x0;
		  else if (is_fixnum (x0))
		    acc = make_flonum (fixnum_value (x0));
		  else
		    vm_type_error (x0); }
p1_inexactTOexact:
    {
      if (is_flonum (x0)) 
	{
	  /* R4RS doesn't say how to round 0.5 here; this is convenient: */
	  double d = floor (flonum_value (x0) + 0.5);
	  if (d + 1 < FIXNUM_MIN || FIXNUM_MAX < d - 1) 
	    {
	      acc = x0;
	      error_msg = "No exact number corresponding to";
	      goto vm_error_label;
	    }
	  acc = make_fixnum ((Fixnum) flonum_value (x0));
	}
      else if (is_fixnum (x0))
	acc = x0;
      else
        vm_type_error (x0);
    }


p1_integerTOchar:	{ callout1 (integer_to_char); }
p1_charTOinteger:	{ vm_check_type (is_char (x0), x0);
			  acc = make_fixnum (char_value (x0)); }
p1_car:			{ vm_check_type (is_pair (x0), x0);
			  acc = car (x0); }
p1_cdr:			{ vm_check_type (is_pair (x0), x0);
			  acc = cdr (x0); }
p1_close_input_port:	{ callout1 (close_input_port); }
p1_close_output_port:	{ callout1 (close_output_port); }
p1_open_input_file:	{ callout1 (open_input_file); }
p1_open_output_file:	{ callout1 (open_output_file); }
p1_stringTOsymbol:	{ vm_check_type (is_string (x0), x0);
			  acc = string_to_symbol (x0); }
p1_symbolTOstring:	{ vm_check_type (is_symbol (x0), x0);
			  acc = string_copy (symbol_to_string (x0)); }
p1_string_length:	{ vm_check_type (is_string (x0), x0);
			  acc = make_fixnum (string_length (x0)); }
p1_vector_length:	{ vm_check_type (is_vector (x0), x0);
			  acc = make_fixnum (vector_length (x0)); }
p1_sqrt: 		{ acc = float_op1 (sqrt, x0); }
p1_exp: 		{ acc = float_op1 (exp,  x0); }
p1_log: 		{ acc = float_op1 (log,  x0); }
p1_sin: 		{ acc = float_op1 (sin,  x0); }
p1_cos: 		{ acc = float_op1 (cos,  x0); }
p1_tan: 		{ acc = float_op1 (tan,  x0); }
p1_asin: 		{ acc = float_op1 (asin, x0); }
p1_acos: 		{ acc = float_op1 (acos, x0); }
p1_atan: 		{ acc = float_op1 (atan, x0); }
p1_closureTOlex_env:    { vm_check_type (is_closure (x0), x0);
			  acc = closure_lex_env (x0); }
p1_closureTOcode:	{ vm_check_type (is_closure (x0), x0);
			  acc = closure_code (x0); }
p1_nullP:    		{ acc = make_boolean (is_null (x0)); }
p1_not:         	{ acc = make_boolean (!is_true (x0)); }
p1_char_white_spaceP:   { vm_check_type (is_char (x0), x0);
			  acc = make_boolean (isspace (char_value (x0))); }
p1_reverse: 		{ callout1 (list_reverse); }
p1_length: 		{ callout1 (prim_list_length); }
p1_skip_blanks: 	{ callout1 (prim_skip_blanks); }
p1_flush_input_line:	{ callout1 (prim_flush_input_line); }
p1_read_fasl_header:	{ callout1 (prim_read_fasl_header); }
p1_read_fasl:		{ callout1 (prim_read_fasl); }
p1_exit: 		{ vm_check_type (is_fixnum (x0), x0);
                          exit (fixnum_value (x0)); }
p1_peek_char:
    { 
      vm_check_type (is_input_port (x0), x0);
      if (!port_is_open (x0)) 
	{
	  acc = x0;
	  goto closed_port_error_label;
	}
      { 
	FILE *fp = port_file (x0);
	int c = fgetc (fp);
	ungetc (c, fp);
	if (c != EOF)
	  acc = make_char ((Char) c); 
	else if (ferror (fp)) 
	  {
	    acc = x0;
	    goto io_error_label;
	  }
	else
	  acc = obj_eof;
      }
    }

p1_read_char:
    { 
      vm_check_type (is_input_port (x0), x0);
      if (!port_is_open (x0)) 
	{
	  acc = x0;
	  goto closed_port_error_label;
	}
      { 
	FILE *fp = port_file (x0);
	int c = fgetc (fp);
	if (c != EOF)
	  acc = make_char ((Char) c); 
	else if (ferror (fp)) 
	  {
	    acc = x0;
	    goto io_error_label;
	  }
	else
	  acc = obj_eof;
      }
    }

p1_listTOvector: { callout1 (list_to_vector); }

p1_system:      { vm_check_type (is_string (x0), x0);
                  acc = make_fixnum (system (string_to_c (x0))); }


p2_eqP:		{ acc = make_boolean (x1 == x0); }
p2_eqvP:	{ acc = make_boolean (eqv (x1, x0)); }
p2_stringEQP:	{ acc = make_boolean (string_equal (x1, x0)); }
p2_modulo:	{ callout2 (modulo); }
p2_quotient:	{ callout2 (quotient); }
p2_remainder:	{ callout2 (my_remainder); }
p2_cons:	{ acc = cons (x1, x0); }
p2_set_carB:	{ vm_check_type (is_pair (x1), x1);
		  field_set (x1, 0, x0); }
p2_set_cdrB:	{ vm_check_type (is_pair (x1), x1);
		  field_set (x1, 1, x0); }
p2_string_ref:	{ vm_check_type (is_string (x1), x1);
		  vm_check_type (is_fixnum (x0), x0);
		  { unsigned i = fixnum_value (x0);
		    if (string_length (x1) <= i)
		      vm_range_error (i);
		    acc = make_char (string_ptr (x1) [i]); } }
p2_vector_ref:	{ vm_check_type (is_vector (x1), x1);
		  vm_check_type (is_fixnum (x0), x0);
		  { unsigned i = fixnum_value (x0);
		    if (vector_length (x1) <= i)
                      vm_range_error (i);
		    acc = vector_ref (x1, i); } }
p2_make_closure:{ vm_check_type (is_vector (x1), x1);
		  vm_check_type (is_vector (x0), x0);
		  acc = make_closure (x1, x0); }
p2_expt: 	{ callout2 (expt); }
p2_atan: 	{ callout2 (prim_atan); }
p2_assq: 	{ callout2 (assq); }
p2_assv: 	{ callout2 (assv); }
p2_charLTP: 	{ vm_check_type (is_char (x1), x1);
                  vm_check_type (is_char (x0), x0);
		  acc = make_boolean (char_value (x1) < char_value (x0)); }
p2_charLEP: 	{ vm_check_type (is_char (x1), x1);
                  vm_check_type (is_char (x0), x0);
		  acc = make_boolean (char_value (x1) <= char_value (x0)); }
p2_charEQP: 	{ vm_check_type (is_char (x1), x1);
                  vm_check_type (is_char (x0), x0);
		  acc = make_boolean (char_value (x1) == char_value (x0)); }
p2_read_atom: 	{ callout2 (prim_read_atom); }
p2_ATdisplay_string: { goto bad_opcode_label; }
p2_write_char:  { vm_check_type (is_char (x1), x1);
		  vm_check_type (is_output_port (x0), x0);
		  if (!port_is_open (x0)) 
		    {
		      acc = x0;
		      goto closed_port_error_label;
		    }
		  if (EOF == fputc (char_value (x1), port_file (x0)))
		    {
		      acc = x0;
		      goto io_error_label;
		    }
		  /* FIXME: should fflush() here, but it'd really kill
		     performance... */
		}


#define COERCE_DOUBLE(d, x)			\
    do {					\
      Object tmp_ = (x);			\
      if (is_flonum (tmp_))			\
	(d) = flonum_value (tmp_);		\
      else if (is_fixnum (tmp_))		\
	(d) = fixnum_value (tmp_);		\
      else {					\
        error_msg = "Bad argument type";	\
        acc = tmp_;				\
        goto vm_error_label;			\
      }						\
    } while (0)

p2_ADD:		
/* FIXME: what about overflow, etc., in the flonum operations? */
/* FIXME: move the dependence on fixnum rep elsewhere... */
/* also this doesn't look any faster than the obvious code.  bleah. 
   maybe we can combine the overflow check with the tag check. */
    { 
      if (is_fixnum (x1) && is_fixnum (x0)) 
	{
				/* tag trickery */
	  Word b1 = (Word) object_bits (x1), b0 = (Word) object_bits (x0);
	  Word sum = b1 + b0 - 0x01;
	  Word bitdiff = b1 ^ b0;
	  if (((bitdiff | ~(bitdiff | (sum ^ b0))) & WORD_HIGHBIT) != 0)
	    acc = (Object) sum;
	  else
	    acc = make_flonum (ashr2 (b1) + ashr2 (b0));
	} 
      else 
	{
	  double d1, d0;
	  COERCE_DOUBLE (d1, x1);
	  COERCE_DOUBLE (d0, x0);
	  acc = make_flonum (d1 + d0);
	}
    }

p2_SUB:
    {
      if (is_fixnum (x1) && is_fixnum (x0)) 
	{                        /* tag trickery */
	  Word b1 = (Word) object_bits (x1), b0 = (Word) object_bits (x0);
	  Word d = b1 - b0 + 0x01;
	  if (((b1 ^ b0) & (b1 ^ d) & WORD_HIGHBIT) == 0)
	    acc = (Object) d;
	  else
	    acc = make_flonum (ashr2 (b1) - ashr2 (b0));
	} 
      else 
	{
	  double d1, d0;
	  COERCE_DOUBLE (d1, x1);
	  COERCE_DOUBLE (d0, x0);
	  acc = make_flonum (as_double (x1) - as_double (x0));
	}
    }

p2_MUL:		{ callout2 (multiply); }
p2_DIV:		{ callout2 (divide); }

p2_LT:
    {
      if (is_fixnum (x1) && is_fixnum (x0))
	/* tag trickery */
	acc = make_boolean ((Word) object_bits (x1) < (Word) object_bits (x0));
      else 
	{
	  double d1, d0;
	  COERCE_DOUBLE (d1, x1);
	  COERCE_DOUBLE (d0, x0);
	  acc = make_boolean (d1 < d0);
	}
    }

p2_LE:
    {
      if (is_fixnum (x1) && is_fixnum (x0))
	/* tag trickery */
	acc = make_boolean ((Word) object_bits (x1) <= (Word) object_bits (x0));
      else 
	{
	  double d1, d0;
	  COERCE_DOUBLE (d1, x1);
	  COERCE_DOUBLE (d0, x0);
	  acc = make_boolean (d1 <= d0);
	}
    }

p2_EQ:
    {
      if (is_fixnum (x1) && is_fixnum (x0))
	/* tag trickery */
	acc = make_boolean ((Word) object_bits (x1) == (Word) object_bits (x0));
      else 
	{
	  double d1, d0;
	  COERCE_DOUBLE (d1, x1);
	  COERCE_DOUBLE (d0, x0);
	  acc = make_boolean (d1 == d0);
	}
    }


#define must_be_natnum(obj) 			\
    do {					\
      Object nat_ = (obj);			\
      if (!is_natnum (nat_))			\
        vm_type_error (nat_); 			\
    } while (0)


p2_make_vector: { must_be_natnum (x1);
		  acc = make_vector (fixnum_value (x1), x0); }
p2_make_string: { must_be_natnum (x1);
		  vm_check_type (is_char (x0), x0);
		  { unsigned L = fixnum_value (x1);
		    acc = make_string (L);
		    memset (string_ptr (acc), char_value (x0), L); } }
p2_numberTOstring:
                { callout2 (prim_number_to_string); }
p2_stringTOnumber:
                { callout2 (prim_string_to_number); }
p2_memq: 	{ callout2 (memq); }
p2_memv: 	{ callout2 (memv); }
p2_append: 	{ callout2 (append); }
p2_write: 	{ callout2 (prim_write); }
p2_display: 	{ callout2 (prim_display); }

p3_string_setB:	{ vm_check_type (is_string (x2), x2);
		  vm_check_type (is_fixnum (x1), x1);
		  vm_check_type (is_char (x0), x0);
		  { unsigned i = fixnum_value (x1);
		    if (string_length (x2) <= i)
		      vm_range_error (i);
		    string_ptr (x2) [i] = char_value (x0); } }
p3_vector_setB: { vm_check_type (is_vector (x2), x2);
                  vm_check_type (is_fixnum (x1), x1);
		  { unsigned i = fixnum_value (x1);
		    if (vector_length (x2) <= i)
                      vm_range_error (i);
		    vector_set (x2, i, x0); } }


drop:				/*  */
  {
    drop ();
  }

halt:				/*  */
  {
    return top ();
  }
