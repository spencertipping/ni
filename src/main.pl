ni('sh:ls') * 'length %0' % '%0 & 1' + '< ../README.md'
  > \*STDOUT;
((ni('< ../README.md') | 'wc -l') + 'sh:echo hi')->into(\*STDOUT);

print ni::self, "\n";
