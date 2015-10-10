# JVM JIT using maven as the frontend
# (this makes it easy to include dependencies)

# Takes a series of -D options -- basically everything after the first two
# given to jit_mvn -- and produces a POM XML file to stdout.
mvn_pom() {
  : TODO
}

# Usage: jit_mvn jit_program Foobar [-Dgroup/artifact=version ...] ... <<'EOF'
# public class Foobar {
#   public static void main(String[] args) {
#     System.out.println("hello world!");
#   }
# }
# EOF
#
# $jit_program "$@"
#
# jit_mvn_free $jit_program
jit_mvn() {
  jit_mvn_result=$1
  jit_mvn_main=$2
}
