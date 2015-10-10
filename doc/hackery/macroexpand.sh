macroexpand_for_posterity() {
  # NB: broken because nested {} will produce invalid heredocs
  eval "$(sed 's/^\(.*\S.*\)\(%(\|%)\)/\1\n\2/g
               s/\(%(\|%)\)\(.*\S.*\)$/\1\n\2/g' \
          | sed '/%(/,/%)/ !{ s/\([\\'\'']\)/'\''\\\1'\''/g
                              s/^/echo '\''/
                              s/$/'\''/ }
                 /%(/,/%)/  { s/{$/<<'\''EOF'\''/
                              s/^\s*}/EOF/ }
                 /%(\|%)/   d')"
}
