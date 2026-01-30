#!/bin/gawk -f 

FNR == 1 { ++route_number; }

# Strip out no-bikes-allowed markup -- but we'll want to use it later.
{ gsub(/<font color=red>/, ""); }
{ gsub("</font>", ""); }

/<pre>/ { headings = 1; next; }

headings {
  headings = 0;
  num_stations = NF;
  for (i = 1; i <= NF; ++i)
    if ($i == "BIKES") {
      num_stations = i;
      break;
    } else 
      names[i] = $i;
}

/[0-9]:[0-9][0-9][ap]/ {
  ++ride_number;
  printf("ride %d %d\n", ride_number, route_number);

  for (i = 1; i <= num_stations; ++i)
    if ($i ~ /^1?[0-9]:[0-9][0-9][ap]$/)
      stops[i] = parse_time($i);
    else
      delete stops[i];

  for (i = 1; i < num_stations; ++i)
    if (i in stops && (n = next_i(i)))
      printf("arc %d %s %d -> %s %d\n",
	     ride_number, names[i], stops[i], names[n], stops[n]);
}

function next_i(i,   j)
{
  for (j = i+1; j <= num_stations; ++j)
    if (j in stops)
      return j;
  return 0;
}

function parse_time(s,   pm, A, hours, minutes)
{
  pm = (s ~ /p$/);
  s = substr(s, 1, length(s) - 1);
  split(s, A, /:/);
  hours = A[1] % 12;
  minutes = A[2];
  if (pm) hours += 12;
  return 3600 * hours + 60 * minutes;
}
