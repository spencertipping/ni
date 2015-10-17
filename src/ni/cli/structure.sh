# Syntactic structures and multimethods
# See ni/ni.sh for the option parser and pipeline compiler; you'd most likely
# use those functions rather than anything here.

meta_hook <<'EOF'
defstruct quasifile name

# Complex commands
defstruct --no-str lambda body          # [ ... ] or ^x
defstruct --no-str lambdafork body      # @[ ... ] or @^x
defstruct --no-str lambdaplus body      # -[ ... ]
defstruct --no-str lambdamix body       # -@[ ... ]
defstruct --no-str branch branches      # { x ... , y ... , ... }
defstruct --no-str branchfork branches  # @{ x ... , y ... , ... }
defstruct --no-str branchsub branches   # -{ ... }
defstruct --no-str branchmix branches   # -@{ ... }

# Pipeline compilation multimethods
defmulti compile                        # compile to a shell command
defmulti describe                       # plain-English description
EOF

for structure_t in lambda lambdafork lambdaplus lambdamix; do
  eval "${structure_t}_str() {
          body ${structure_t}_str_b \$2
          str ${structure_t}_str_s \$${structure_t}_str_b
          eval \"\$1=\\\"<${structure_t} \\\$${structure_t}_str_s>\\\"\"
        }"
done

for structure_t in branch branchfork branchsub branchmix; do
  eval "${structure_t}_str() {
          branches ${structure_t}_str_b \$2
          str ${structure_t}_str_s \$${structure_t}_str_b
          eval \"\$1=\\\"<${structure_t} \\\$${structure_t}_str_s>\\\"\"
        }"
done
