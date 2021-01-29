# ni python stream driver.
# See core/pl/stream.pm for definitions.

import re

a, b, c, d, e, f, g, h, i, j, k, l = (None,) * 12
F = []
FM = -1
_ = None

def r(*fs):
  print('\t'.join([re.sub('\n', '', str(s)) for s in fs]))

def rl():
  # TODO: add multiline support
  global a, b, c, d, e, f, g, h, i, j, k, l, _, F, FM
  try:
    _ = next(stdin)
    if _[-1] == '\n':
      _ = _[0:-1]
  except StopIteration:
    _ = None

  F = re.split('\t', _ or '')
  a, b, c, d, e, f, g, h, i, j, k, l, *_F = F + [None] * max(0, 12 - len(F))
  FM = len(F) - 1
  return _
