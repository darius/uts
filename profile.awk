#!/usr/bin/awk -f

BEGIN {				# Read in the table of op names.
  bop = 0;
  prim = -1;
  while (getline line <"instrucs.c" > 0) {
    if (match(line, /^p[0-3]_[A-Za-z0-9_]*:/)) {
      p = substr(line, 2, 1);
      if (p != prim) {
	prim = p;
	name[bop++] = "prim-" p;
      }
    } else if (match(line, /^[A-Za-z0-9_]*:/)) {
      name[bop++] = substr(line, 0, RLENGTH - 1);
    }
  }
  if (bop != 23)
    print "error!"
  name[bop++] = "varref/0"
}

{ count[$1, $2] = $3; lsum[$1] += $3; rsum[$2] += $3; }

END {
  if (totals_by_op) {
    for (i in rsum) {
      printf("%-17s %10.10u\n",
	     name[i], rsum[i]);
    }
  } else {
    for (i in count) {
      s = index(i, SUBSEP);
      L = substr(i, 1, s-1);
      R = substr(i, s+1);
      printf("%-17s %-17s %10u %5.1f %5.1f\n", 
	     name[L], name[R], count[i], 
	     percent(count[i], rsum[R]),
	     percent(count[i], lsum[L])); 
    }
  }
}

function percent(p, total)
{
  return total == 0 ? 0 : 100 * p/total;
}
