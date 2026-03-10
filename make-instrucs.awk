# Build instruc-cases.c from instrucs.c

# A line with a C stmt label, not preceded by whitespace, is taken to
# start the code for an instruction or primitive:
#   p0_foo: primitive with 0 operands, named foo.
#   p3_bar: ditto with 3 operands
#   baz (no p<n> prefix): bytecode instruction named baz.

# We replace this label with "case <k>:" where k is autoincremented.
# The k's are incremented independently for the different cases above
# (i.e. the bytecode instruction numbering is separated from the
# primitive numberings, as well as independent numberings for the
# different operand counts).

# We also auto-insert "break;" between these cases.

# We generate the skeleton of the prim_<k> instructions:
#   Object x0 = pop ();
#   ...
#   switch (get_byte ()) {
#      ...
#      <cases such as p1_foo> as described above
#      ...
#      default: vm_error ("Bad opcode", nil);
#   }
#   push (acc);

# Plus a callout block to implement e.g. { callout1 (integer_to_char); }

# Admittedly this expansion doesn't streamline the code much, and 
# I guess makes it harder to understand. What I had in mind at the time
# included more build-time optimization work along the lines of vmgen.


BEGIN { cur = "beginit"; }

match($0, /^p[0-3]_[A-Za-z0-9_]*:/) {
  flush_current(substr($0, 2, 1));
  start_new(substr($0, 2, 1));
  printf("%*s", RLENGTH, "");
  print substr($0, RLENGTH + 1);
  next;
}

match($0, /^[A-Za-z_][A-Za-z_0-9]*:/) {
  flush_current("");
  start_new("");
  printf("%*s", RLENGTH, "");
  print substr($0, RLENGTH + 1);
  next;
}

{ print }

END { flush_current("endit"); }

function start_new(c,    i)
{
  cur = c;
  counter[cur] += 0;
  if (cur != "" && counter[cur] == 0) {
    printf("  case %d:\n", counter[""]++);
    printf("    {\n")
    for (i = 0; i < cur; ++i)
      printf("      Object x%d = pop ();\n", i);
    if (has_callouts(cur))
      printf("      callout%d_t c_routine;\n", cur);
    printf("\n");
    printf("      switch (get_byte ()) {\n");
  }
  printf("case %d:\n", counter[cur]++);
}

function flush_current(c)
{
  if (cur != c && cur ~ /^[0-3]$/) {
    if (c != 0) {
      printf("        break;\n");
      printf("      default: vm_error (\"Bad opcode\", nil);\n");
      printf("      }\n");
      printf("    push (acc);\n");
      if (has_callouts(cur)) {
	printf("    break;\n");
	printf("\n");
	printf("  callout%d_label:\n", cur);
	printf("    flush_registers ();\n");
	printf("    acc = (*c_routine) (%s);\n", callout_arglist(cur));
	printf("    push (acc);\n");
      }
      printf("  }\n");
    }
  }
  if (cur != "beginit") {
    printf("  break;\n");
    printf("\n");
  }
}

function has_callouts(c)
{
  return c == 1 || c == 2;
}

function callout_arglist(c)
{
  if (c == 1)
    return "x0";
  if (c == 2)
    return "x1, x0";
}
