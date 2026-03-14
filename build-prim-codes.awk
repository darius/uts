# Generate primcodes.h and primcodes.scm from prims

/^#/ { next; }
NF == 0 { next; }

NF != 2 { print "bad spec line" NR; print; exit(1); }

{
    arity = $1; sname = $2;

    cname = sname;
    if (cname == "-") cname = "SUB";
    else {
        gsub(/->/, "TO", cname);
        gsub(/<=/, "LE", cname);
        gsub(/>=/, "GE", cname);
        gsub(/</, "LT", cname);
        gsub(/>/, "GT", cname);
        gsub(/=/, "EQ", cname);

        gsub(/-/, "_", cname);
        gsub(/[?]/, "P", cname);
        gsub(/[+]/, "ADD", cname);
        gsub(/[*]/, "MUL", cname);
        gsub(/\//, "DIV", cname);
        gsub(/!/, "B", cname);
        gsub(/%/, "", cname);   # TODO define a replacement
        gsub(/@/, "AT", cname);
    }

    n = count[arity]++;
    snames[arity, n] = sname;
    cnames[arity, n] = cname;
}

END {
    # C output
    cfile = "prims.h";
    for (arity = 0; arity <= 3; ++arity) {
        printf("enum prim_%d_op {\n", arity) >cfile;
        for (i = 0; i < count[arity]; ++i) {
            printf("  p%d_%s,\n", arity, cnames[arity, i]) >cfile;
        }
        print "};" >cfile;
        print "" >cfile;
    }

    # Scheme output
    sfile = "primcodes.scm";
    for (arity = 0; arity <= 3; ++arity) {
        printf("(define prim-%d-list\n", arity) >sfile;
        printf("  '(\n", arity) >sfile;
        for (i = 0; i < count[arity]; ++i) {
            printf("    (%-20s %d)\n", snames[arity, i], i) >sfile;
        }
        printf("   ))\n", arity) >sfile;
        print "" >sfile;
    }
}
