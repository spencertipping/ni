# Changes and why they happened
### 2017.0211: Remove the `/` prefix from ni namespaces
Prior to this change, ni used package names like `/object` and `/lib/fn`. I
would have preferred that (since they're visually distinct from normal perl
classes), but Perl 5.14 and prior don't support method calls on packages
beginning with `/`. This caused test failures on centos 5-6 and ubuntu 12.04.
