# Misc invariants

## Murmurhash probabilistic invariant
The murmurhash32 function should produce good distribution over its output
range, even if we consider just a subset of its output bits.

```bash
$ ni nE5 ,h U wcl
99999
$ ni nE5 ,h p'a & 0xffff' U wcl     # low 16 bits
51345
$ ni nE5 ,h p'a >> 16' U wcl        # high 16 bits
51344
```
