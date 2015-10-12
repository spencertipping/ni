# ni frontend core definitions: syntactic structures and multimethods
# See also meta/ni-option.sh for the metaprogramming used by conf.sh.

# Complex commands
defstruct lambda body           # [ ... ] or ^x
defstruct lambdafork body       # @[ ... ] or @^x
defstruct lambdaplus body       # -[ ... ]
defstruct lambdamix body        # -@[ ... ]
defstruct branch map            # { x ... , y ... , ... }
defstruct branchfork map        # @{ x ... , y ... , ... }
defstruct branchsub map         # -{ ... }
defstruct branchmix map         # -@{ ... }

# Pipeline compilation multimethods
# TODO

ni_compile() TODO ni_compile
ni_parse_options() TODO ni_parse_options
