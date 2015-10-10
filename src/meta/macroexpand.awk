BEGIN {
  macro_mode = 0
  n = -1
}

function await(c)       { awaiting[++n] = c }
function is_quoted()    { return n >= 0 && awaiting[n] ~ /["']/ }
function shell_quote(s) { gsub(/[\\']/, "'\\\\&'", s);
                          return "printf %s '" s "'" }

function macro_process(s, r, c, q, heredoc_start, heredoc_end, last_was_dollar) {
  r               = ""
  last_was_dollar = 0
  heredoc_start   = macro_mode ? "\n" : " <<'macro_end'"
  heredoc_end     = macro_mode ? "\nmacro_end\n" : "\n"
  macro_mode      = 1
  for (i = 1; i <= length(s); ++i) {
    c = substr(s, i, 1)
    q = is_quoted()
    if (n >= 0 && c == awaiting[n]) {
      --n
    } else if (n == -1 && c == ")") {
      macro_mode = 0
      if (i + 1 <= length(s)) {
        return r heredoc_end shell_quote(substr(s, i + 1))
      } else {
        return r
      }
    } else if (c == "\\") {
      ++i
    } else if (c == "(" && (last_was_dollar || !q)) {
      await(")")
    } else if (c == "{") {
      await("}")
    } else if (c == "\"" || c == "'") {
      await(c)
    }
    last_was_dollar = c == "$"
    r = r c
  }
  sub(/\n$/, "", r)
  return r heredoc_start
}

{
  if (macro_mode) {
    print macro_process($0 "\n")
  } else {
    if ((p = index($0, "%(")) > 0) {
      print shell_quote(substr($0, 1, p - 1))
      print macro_process(substr($0, p + 2) "\n")
    } else {
      print shell_quote($0 "\n")
    }
  }
}
