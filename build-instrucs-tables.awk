# Generate opcodes.h and opcodes.scm from instrucs

/^#/ { next; }

NF != 2 && NF != 3 { print "bad spec line"; print; exit(1); }

{
    opnum += 0;
    sname[opnum] = $1;
    cname[opnum] = $2;
    argspec[opnum] = $3;
    ++opnum;
}

END {
    cfile = "opcodes.h";

    print "enum byteop {" >cfile;
    for (i = 0; i < opnum; ++i) {
        printf("  bop_%s,\n", cname[i]) >cfile;
    }
    print "};" >cfile;
    print "" >cfile;

    sfile = "opcodes.scm";

    for (i = 0; i < opnum; ++i) {
        printf("(define @%%%s %d)\n", sname[i], i) >sfile;
    }
    print "" >sfile;

    print "(define instruc-names" >sfile;
    printf("  '#(") >sfile;
    for (i = 0; i < opnum; ++i) {
        printf(" %s", sname[i]) >sfile;
    }
    print "))" >sfile;
    print "" >sfile;

    print "(define @instruc-args" >sfile;
    printf("  '#(") >sfile;
    for (i = 0; i < opnum; ++i) {
        printf(" (%s)", argspec[i]) >sfile;
    }
    print "))" >sfile;
    print "" >sfile;


}
