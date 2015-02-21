ni('sh:ls') > \*STDOUT;
ni('sh:ls') + 'sh:ls' > \*STDOUT;
ni('sh:ls') + '< ../README.md' + 'sh:ls' > \*STDOUT;
ni('< ../README.md') + 'sh:echo hi' > \*STDOUT;

ni('sh:ls') * 'length %0' % '%0 & 1' + '< ../README.md'
  > \*STDOUT;

(ni('< ../README.md') | 'wc -l') + 'sh:echo hi' > '> output';
