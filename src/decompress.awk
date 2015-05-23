BEGIN { print "static const char *const q[];" }
{
  print $0
  gsub("\\\\", "\\\\\\\\")
  gsub("\"", "\\\"")
  q[NR] = "\"" $0 "\""
  last_q = NR
}

END {
  print "static const char *const q[] = {"
  for (i = 1; i <= last_q; i++) {
    print q[i] ","
  }
  print "(const char*) NULL};"
}
