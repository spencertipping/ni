# JVM JIT using maven as the frontend

# POM generation stuff
# The goal here is to make it trivial to build a working POM file, without
# typing out a whole bunch of XML. This is done by defining POM-macro functions
# that handle various command-line options and generate the relevant XML
# fragments for you. For example, here's the dependency generator:

mvn_pom_deps() {
  verb '<dependencies>'
  for mvn_pom_deps_d; do
    # Dependencies are specified using this syntax:
    # [groupId/]artifactId=version[:scope]
    mvn_pom_deps_ga="${1%%=*}"
    mvn_pom_deps_vs="${1##*=}"
    mvn_pom_deps_g="${mvn_pom_deps_ga%%/*}"
    mvn_pom_deps_s="${mvn_pom_deps_vs##*:}"
    [ "$mvn_pom_deps_s" = "$mvn_pom_deps_vs" ] \
      && mvn_pom_deps_s= \
      || mvn_pom_deps_s="<scope>$mvn_pom_deps_s</scope>"
    verb "<dependency>" \
         "<groupId>$mvn_pom_deps_g</groupId>" \
         "<artifactId>${mvn_pom_deps_ga##*/}</artifactId>" \
         "<version>${mvn_pom_deps_vs%%:*}</version>" \
         "$mvn_pom_deps_s</dependency>"
  done
  verb '</dependencies>'
}

# Calling syntax for the above would be this:
#
# mvn_pom --deps foo.bar/bif=1.0.0-SNAPSHOT:development \
#                foo.bar/baz=10:test \
#         --...
#
# ni detects the end of arguments to a generator by looking for the next option
# starting with --.

mvn_pom() {
  : TODO
}

# Usage: jit_program=$(jit_mvn Foobar [-Dgroup/artifact=version ...] ... <<'EOF'
# public class Foobar {
#   public static void main(String[] args) {
#     System.out.println("hello world!");
#   }
# }
# EOF
# )
#
# $jit_program "$@"
# jit_mvn_free $jit_program
jit_mvn() {
  jit_mvn_result=$1
  jit_mvn_main=$2
}
