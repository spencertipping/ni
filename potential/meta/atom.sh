# Atoms
meta_hook <<'EOF'
defstruct string x
defstruct atom   x
EOF

string_str() eval "$1=\"\$${2}_x\""
atom_str()   eval "$1=\"\$${2}_x\""
