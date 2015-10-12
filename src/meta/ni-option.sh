# ni option data structure and generator

meta_hook <<'EOF'
defstruct option long short syntax fn description
hashmap long_options
hashmap short_options
EOF

option_str() eval "$1=\"-\$${2}_short|--\$${2}_long [\$${2}_syntax]" \
                  "\$${2}_fn(): \$${2}_description\""

# Defines a long and short option, with parsing hints. For example:
# defoption --group -g D group "groups rows by column value"
#
# This means the following:
# --group       long option name is "--group"; this is required
# -g            short option name is "-g"; can be '' for no short option
# D             option takes an optional number as an argument
# ni_group      compiler function to be called for the option
# "groups ..."  usage for the option
#
# If you then invoke ni as "ni -g4", "ni_group" will be called on "4" to
# interact with the compiler to generate the relevant shell script. See ni.sh
# for details about how this works.
defoption() {
  defoption_long="${1#--}"
  defoption_short="${2#-}"
  option defoption_new $defoption_long "$defoption_short" "$3" "$4" "$5"
  assoc $long_options $defoption_long $defoption_new
  [ -n "$defoption_short" ] \
    && assoc $short_options $defoption_short $defoption_new
}
