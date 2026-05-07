/* to be #included in uts.c */
/* is get_byte signed, or what? */

break; case bop_lit:
  { 
    need (1);
    push (get_datum ()); 
  }

break; case bop_varref:
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

break; case bop_varset:
  {
    unsigned frame = get_byte ();
    unsigned offset = get_byte ();
    *lookup_lex_env (lex_env, frame, offset) = top ();
  }

break; case bop_global_ref:
  {
    Object var = get_datum ();
    Object value = global_value (var);
    if (is_unbound (value))
      {
	acc = var;
	goto unbound_error_label;
      }
    need (1);
    push (value);
  }

break; case bop_global_set:
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

break; case bop_global_define:
  {
    Object var = get_datum ();
    set_global_value(var, top ());
    set_top (var);
  }

break; case bop_unless:
  {
    if (is_true (pop ()))
      pc += 2;
    else 
      {
	unsigned offset = get_short ();
	pc += offset;
      }
  }

break; case bop_jump:
  {
    unsigned offset = get_short ();
    pc += offset;
  }

break; case bop_proc:
  {
    need (1);
    push (make_closure (lex_env, get_datum ()));
  }

break; case bop_params:
  {
    unsigned num_formals = get_byte ();
    int num_args = stack_ptr - frame_ptr; /* assumes stack grows upward */
    if (num_args != num_formals)
      { 
        Object arguments = nil;
        while (frame_ptr < stack_ptr)
          arguments = cons (pop (), arguments);
        // Now the stack state is popped back to the continuation.
        // Cons an informative irritant:
        // want: (calling: #<procedure h> nargs: 1 required-nargs: 2 arguments: (1) called-from: #<procedure g> byte-offset: 42) but we don't have all that info anymore
        acc = cons (c_symbol ("nargs:"),
                    cons (make_fixnum (num_args),
                          cons (c_symbol ("required-nargs:"),
                                cons (make_fixnum (num_formals),
                                      cons (c_symbol ("callee:"),
                                            cons (make_closure (lex_env, code), // TODO sucks that we have no handle on the original closure object at this point
                                                  cons (c_symbol ("arguments:"),
                                                        cons (arguments,
                                                              nil))))))));
        restore_state ();
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

break; case bop_rest_params:
  {
    unsigned min_num_args = get_byte ();
    unsigned num_args = stack_ptr - frame_ptr;
    if (num_args < min_num_args) 
      {
        Object arguments = nil;
        while (frame_ptr < stack_ptr)
          arguments = cons (pop (), arguments);
        // Now the stack state is popped back to the continuation.
        // Cons an informative irritant:
        // want: (calling: #<procedure h> nargs: 1 min-nargs: 2 arguments: (1) called-from: #<procedure g> byte-offset: 42) but we don't have all that info anymore
        acc = cons (c_symbol ("nargs:"),
                    cons (make_fixnum (num_args),
                          cons (c_symbol ("min-nargs:"),
                                cons (make_fixnum (min_num_args),
                                      cons (c_symbol ("callee:"),
                                            cons (make_closure (lex_env, code),
                                                  cons (c_symbol ("arguments:"),
                                                        cons (arguments,
                                                              nil))))))));
        restore_state ();
        error_msg = "Too few arguments";
        goto vm_error_label;
      } 
    else 
      {
        Object new_lex_env = allot_vector (1 + 1 + min_num_args);
        vector_set (new_lex_env, 0, lex_env);
        {
          unsigned i;
          Object rest_args = nil;
          for (i = num_args - min_num_args; i != 0; --i)
            rest_args = cons (pop (), rest_args);
          vector_set (new_lex_env, 1 + min_num_args, rest_args);
          for (i = min_num_args; i != 0; --i)
            vector_set (new_lex_env, i, pop ());
          lex_env = new_lex_env;
        }
      }
  }

break; case bop_restore:
  {
    Object result = pop ();
    restore_state ();
    push (result);
  }

break; case bop_invoke:
  {
    acc = pop ();
  apply_proc:
    if (!is_closure (acc)) 
      {
        // Put the arguments in a list for the irritant:
        Object arguments = nil;
        while (frame_ptr < stack_ptr)
	  arguments = cons (pop (), arguments);
        // TODO make a function to listify like that
        // Now the stack state is popped back to the continuation.
        // Cons an informative irritant:
        acc = cons (c_symbol ("calling:"),
                    cons (acc,
                          cons (c_symbol ("arguments:"),
                                cons (arguments,
                                      nil))));
        // Raise the error:
        error_msg = "Call to a non-procedure";
	goto vm_error_label;
      }
    pc = 0;
    code = closure_code (acc);
    lex_env = closure_lex_env (acc);
    constants = vector_ptr (vector_ref (code, 1));
    bvec = string_ptr (vector_ref (code, 2));
#ifdef FUNCTION_PROFILING
    {
      Object c = vector_ref (code, 5); // 5 = codevec slot for call count
      if (is_fixnum (c))
	{
	  int count = fixnum_value (c) + 1;
	  if (int_is_fixnum (count))
	    vector_set (code, 5, make_fixnum (count));
	  if (count == 2)	/* don't link in until 2nd hit */
	    set_global_value (
	      all_entered_code_vectors_symbol,
	      cons (code, global_value (all_entered_code_vectors_symbol)));
	}
    }
#endif
  }

break; case bop_save:
  {
    unsigned offset = get_short ();
    save_state (code, lex_env, pc + offset);
  }

break; case bop_apply:
  {
    int num_args = stack_ptr - frame_ptr;  /* assumes stack grows upward */
    if (num_args < 2) 
      { 
        // Put the arguments in a list as the irritant:
        acc = nil;
        while (frame_ptr < stack_ptr)
          acc = cons (pop (), acc);
        // Now the stack state is popped back to the continuation.	
        restore_state (); // XXX why did I write restore_state() here, but not in the corresponding code for bop_invoke?
        acc = cons (c_symbol ("arguments:"), cons (acc, nil));
        error_msg = "Too few arguments to apply";
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
            need (1);
            push (car (rest));
          }

        if (!is_null (rest)) 
          {
            error_msg = "Non-list argument to apply";
            acc = list;
            stack_ptr = frame_ptr;
            restore_state ();
            goto vm_error_label;
          }
        acc = proc;
        goto apply_proc;
      }
  }

break; case bop_get_cc:
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

break; case bop_set_cc:
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

break; case bop_prim_0:
  {
    switch (get_byte ()) {

    default:
        vm_error ("Bad opcode", nil);

    break; case p0_current_input_port:
        { acc = current_input_port; }

    break; case p0_current_output_port:
        { acc = current_output_port; }

    break; case p0_runtime:
        { acc = make_flonum ((clock () - clock_start) / 
                             (double) CLOCKS_PER_SEC); }

    }
    push (acc);
  }

break; case bop_prim_1:
  {
    Object x0 = pop ();
    callout1_t c_routine;
#define callout1(c)   do { c_routine = (c); goto callout1_label; } while (0)

    switch (get_byte ()) {

    default:
        vm_error ("Bad opcode", nil);

    break; case p1_booleanP:
        { acc = make_boolean (is_boolean (x0)); }

    break; case p1_charP:
        { acc = make_boolean (is_char (x0)); }

    break; case p1_eof_objectP:
        { acc = make_boolean (is_eof_object (x0)); }

    break; case p1_exactP:
        { acc = make_boolean (is_fixnum (x0)); }

    break; case p1_inexactP:
        { acc = make_boolean (is_flonum (x0)); }

    break; case p1_input_portP:
        { acc = make_boolean (is_input_port (x0)); }

    break; case p1_integerP:
        { acc = make_boolean (is_fixnum (x0)
                              || (is_flonum (x0)
                                  && flonum_value (x0) == 
                                  floor (flonum_value (x0)))); }

    break; case p1_numberP:
        { acc = make_boolean (is_number (x0)); }

    break; case p1_output_portP:
        { acc = make_boolean (is_output_port (x0));}

    break; case p1_pairP:
        { acc = make_boolean (is_pair (x0)); }

    break; case p1_procedureP:
        { acc = make_boolean (is_closure (x0)); }

    break; case p1_stringP:
        { acc = make_boolean (is_string (x0)); }

    break; case p1_symbolP:
        { acc = make_boolean (is_symbol (x0)); }

    break; case p1_vectorP:
        { acc = make_boolean (is_vector (x0)); }

    break; case p1_floor:
        { if (is_flonum (x0))
                acc = make_flonum (floor (flonum_value (x0)));
            else if (is_fixnum (x0))
                acc = x0;
            else 
                vm_type_error (x0); }

    break; case p1_round:
        { if (is_flonum (x0))
                acc = make_flonum (my_round (flonum_value (x0)));
            else if (is_fixnum (x0))
                acc = x0;
            else
                vm_type_error (x0); }

    break; case p1_exactTOinexact:
        { if (is_flonum (x0))
                acc = x0;
            else if (is_fixnum (x0))
                acc = make_flonum (fixnum_value (x0));
            else
                vm_type_error (x0); }

    break; case p1_inexactTOexact:
        {
          if (is_flonum (x0))
            {
                /* R4RS doesn't say how to round 0.5 here; this is convenient: */
                double d = floor (flonum_value (x0) + 0.5);
                if (d < FIXNUM_MIN || d > FIXNUM_MAX)
                    {
                        acc = x0;
                        error_msg = "No exact number corresponding to";
                        goto vm_error_label;
                    }
                acc = make_fixnum ((Fixnum) d);
            }
          else if (is_fixnum (x0))
              acc = x0;
          else
              vm_type_error (x0);
        }

    break; case p1_integerTOchar:
        { callout1 (integer_to_char); }

    break; case p1_charTOinteger:
        { vm_check_type (is_char (x0), x0);
            acc = make_fixnum (char_value (x0)); }

    break; case p1_car:
        { vm_check_type (is_pair (x0), x0);
            acc = car (x0); }

    break; case p1_cdr:
        { vm_check_type (is_pair (x0), x0);
            acc = cdr (x0); }

    break; case p1_close_input_port:
        { callout1 (close_input_port); }

    break; case p1_close_output_port:
        { callout1 (close_output_port); }

    break; case p1_open_input_file:
        { callout1 (open_input_file); }

    break; case p1_open_output_file:
        { callout1 (open_output_file); }

    break; case p1_stringTOsymbol:
        { vm_check_type (is_string (x0), x0);
            acc = string_to_symbol (x0); }

    break; case p1_symbolTOstring:
        { vm_check_type (is_symbol (x0), x0);
            acc = string_copy (symbol_to_string (x0)); }

    break; case p1_string_length:
        { vm_check_type (is_string (x0), x0);
            acc = make_fixnum (string_length (x0)); }

    break; case p1_vector_length:
        { vm_check_type (is_vector (x0), x0);
            acc = make_fixnum (vector_length (x0)); }

    break; case p1_sqrt:
        { acc = float_op1 (sqrt, x0); }

    break; case p1_exp:
        { acc = float_op1 (exp,  x0); }

    break; case p1_log:
        { acc = float_op1 (log,  x0); }

    break; case p1_sin:
        { acc = float_op1 (sin,  x0); }

    break; case p1_cos:
        { acc = float_op1 (cos,  x0); }

    break; case p1_tan:
        { acc = float_op1 (tan,  x0); }

    break; case p1_asin:
        { acc = float_op1 (asin, x0); }

    break; case p1_acos:
        { acc = float_op1 (acos, x0); }

    break; case p1_atan:
        { acc = float_op1 (atan, x0); }

    break; case p1_closureTOlex_env:
        { vm_check_type (is_closure (x0), x0);
            acc = closure_lex_env (x0); }

    break; case p1_closureTOcode:
        { vm_check_type (is_closure (x0), x0);
            acc = closure_code (x0); }

    break; case p1_nullP:
        { acc = make_boolean (is_null (x0)); }

    break; case p1_not:
        { acc = make_boolean (!is_true (x0)); }

    break; case p1_char_whitespaceP:
        { vm_check_type (is_char (x0), x0);
            acc = make_boolean (isspace (char_value (x0))); }

    break; case p1_reverse:
        { callout1 (list_reverse); }

    break; case p1_length:
        { callout1 (prim_list_length); }

    break; case p1_skip_blanks:
        { callout1 (prim_skip_blanks); }

    break; case p1_flush_input_line:
        { callout1 (prim_flush_input_line); }

    break; case p1_exit:
        { vm_check_type (is_fixnum (x0), x0);
            exit (fixnum_value (x0)); }

    break; case p1_peek_char:
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

    break; case p1_read_char:
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

    break; case p1_listTOvector:
        { callout1 (list_to_vector); }

    break; case p1_system:
        { vm_check_type (is_string (x0), x0);
            acc = make_fixnum (system (string_to_c (x0))); }

    break; case p1_bitwise_not:
        { vm_check_type (is_fixnum (x0), x0);
            acc = make_fixnum (~fixnum_value (x0)); }

    }
    push (acc);
    break;

  callout1_label:
    flush_registers ();
    acc = (*c_routine) (x0);
    push (acc);
  }

break; case bop_prim_2:
    {
      Object x0 = pop ();
      Object x1 = pop ();
      callout2_t c_routine;
#define callout2(c)   do { c_routine = (c); goto callout2_label; } while (0)

      switch (get_byte ()) {

      default:
          vm_error ("Bad opcode", nil);

      break; case p2_eqP:
          { acc = make_boolean (x1 == x0); }

      break; case p2_eqvP:
          { acc = make_boolean (eqv (x1, x0)); }

      break; case p2_stringEQP:
          { acc = make_boolean (string_equal (x1, x0)); }

      break; case p2_modulo:
          { callout2 (modulo); }

      break; case p2_quotient:
          { callout2 (quotient); }

      break; case p2_remainder:
          { callout2 (my_remainder); }

      break; case p2_cons:
          { acc = cons (x1, x0); }

      break; case p2_set_carB:
          { vm_check_type (is_pair (x1), x1);
              field_set (x1, 0, x0); }

      break; case p2_set_cdrB:
          { vm_check_type (is_pair (x1), x1);
              field_set (x1, 1, x0); }

      break; case p2_string_ref:
          { vm_check_type (is_string (x1), x1);
              vm_check_type (is_fixnum (x0), x0);
              { unsigned i = fixnum_value (x0);
                  if (string_length (x1) <= i)
		      vm_range_error (i);
                  acc = make_char (string_ptr (x1) [i]); } }

      break; case p2_vector_ref:
          { vm_check_type (is_vector (x1), x1);
              vm_check_type (is_fixnum (x0), x0);
              { unsigned i = fixnum_value (x0);
                  if (vector_length (x1) <= i)
                      vm_range_error (i);
                  acc = vector_ref (x1, i); } }

      break; case p2_make_closure:
          {
              vm_check_type (is_vector (x1), x1);
              vm_check_type (is_vector (x0), x0);
              acc = make_closure (x1, x0); }

      break; case p2_expt:
          { callout2 (expt); }

      break; case p2_atan:
          { callout2 (prim_atan); }

      break; case p2_assq:
          { callout2 (assq); }

      break; case p2_assv:
          { callout2 (assv); }

      break; case p2_charLTP:
          { vm_check_type (is_char (x1), x1);
              vm_check_type (is_char (x0), x0);
              acc = make_boolean (char_value (x1) < char_value (x0)); }

      break; case p2_charLEP:
          { vm_check_type (is_char (x1), x1);
              vm_check_type (is_char (x0), x0);
              acc = make_boolean (char_value (x1) <= char_value (x0)); }

      break; case p2_charEQP:
          { vm_check_type (is_char (x1), x1);
              vm_check_type (is_char (x0), x0);
              acc = make_boolean (char_value (x1) == char_value (x0)); }

      break; case p2_read_atom:
          { callout2 (prim_read_atom); }

      break; case p2_display_string:
          { goto bad_opcode_label; }

      break; case p2_write_char:
          { vm_check_type (is_char (x1), x1);
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

      break; case p2_ADD:
          {
              if (is_fixnum (x1) && is_fixnum (x0))
                  {
                      Fixnum v1 = fixnum_value (x1), v0 = fixnum_value (x0);
                      Fixnum sum = v1 + v0;  /* can't overflow int64: two 62-bit values */
                      if (int_is_fixnum (sum))
                          acc = make_fixnum (sum);
                      else
                          acc = make_flonum ((double) v1 + (double) v0);
                  }
              else
                  {
                      double d1, d0;
                      COERCE_DOUBLE (d1, x1);
                      COERCE_DOUBLE (d0, x0);
                      acc = make_flonum (d1 + d0);
                  }
          }

      break; case p2_SUB:
          {
              if (is_fixnum (x1) && is_fixnum (x0))
                  {
                      Fixnum v1 = fixnum_value (x1), v0 = fixnum_value (x0);
                      Fixnum diff = v1 - v0;  /* can't overflow int64: two 62-bit values */
                      if (int_is_fixnum (diff))
                          acc = make_fixnum (diff);
                      else
                          acc = make_flonum ((double) v1 - (double) v0);
                  }
              else
                  {
                      double d1, d0;
                      COERCE_DOUBLE (d1, x1);
                      COERCE_DOUBLE (d0, x0);
                      acc = make_flonum (d1 - d0);
                  }
          }

      break; case p2_MUL:
          { callout2 (multiply); }

      break; case p2_DIV:
          { callout2 (divide); }

      break; case p2_LT:
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

      break; case p2_LE:
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

      break; case p2_EQ:
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

      break; case p2_make_vector:
          { must_be_natnum (x1);
              acc = make_vector (fixnum_value (x1), x0); }

      break; case p2_make_string:
          { must_be_natnum (x1);
              vm_check_type (is_char (x0), x0);
              { unsigned L = fixnum_value (x1);
                  acc = make_string (L);
                  memset (string_ptr (acc), char_value (x0), L); } }

      break; case p2_numberTOstring:
          { callout2 (prim_number_to_string); }

      break; case p2_stringTOnumber:
          { callout2 (prim_string_to_number); }

      break; case p2_memq:
          { callout2 (memq); }

      break; case p2_memv:
          { callout2 (memv); }

      break; case p2_append:
          { callout2 (append); }

      break; case p2_write:
          { callout2 (prim_write); }

      break; case p2_display:
          { callout2 (prim_display); }

      break; case p2_bitwise_and:
          { vm_check_type (is_fixnum (x1), x1);
              vm_check_type (is_fixnum (x0), x0);
              acc = make_fixnum (fixnum_value (x1) & fixnum_value (x0)); }

      break; case p2_bitwise_ior:
          { vm_check_type (is_fixnum (x1), x1);
              vm_check_type (is_fixnum (x0), x0);
              acc = make_fixnum (fixnum_value (x1) | fixnum_value (x0)); }

      break; case p2_bitwise_xor:
          { vm_check_type (is_fixnum (x1), x1);
              vm_check_type (is_fixnum (x0), x0);
              acc = make_fixnum (fixnum_value (x1) ^ fixnum_value (x0)); }

      break; case p2_arithmetic_shift:
          { vm_check_type (is_fixnum (x1), x1);
              vm_check_type (is_fixnum (x0), x0);
              Fixnum v = fixnum_value (x1);
              Fixnum n = fixnum_value (x0);
              if (n >= 0)
                  acc = make_fixnum (v << n);
              else {
                  /* portable arithmetic right shift */
                  int s = -n;
                  acc = make_fixnum (v >= 0 ? v >> s : ~(~v >> s));
              } }

      }
      push (acc);
      break;

  callout2_label:
    flush_registers ();
    acc = (*c_routine) (x1, x0);
    push (acc);
  }

break; case bop_prim_3:
  {
      Object x0 = pop ();
      Object x1 = pop ();
      Object x2 = pop ();

      switch (get_byte ()) {

      default:
          vm_error ("Bad opcode", nil);

      break; case p3_string_setB:
          { vm_check_type (is_string (x2), x2);
              vm_check_type (is_fixnum (x1), x1);
              vm_check_type (is_char (x0), x0);
              { unsigned i = fixnum_value (x1);
                  if (string_length (x2) <= i)
		      vm_range_error (i);
                  string_ptr (x2) [i] = char_value (x0); } }

      break; case p3_vector_setB:
          { vm_check_type (is_vector (x2), x2);
              vm_check_type (is_fixnum (x1), x1);
              { unsigned i = fixnum_value (x1);
                  if (vector_length (x2) <= i)
                      vm_range_error (i);
                  vector_set (x2, i, x0); } }

      }
      push (acc);
  }

break; case bop_drop:
  {
    drop ();
  }

break; case bop_halt:
  {
    return top ();
  }
