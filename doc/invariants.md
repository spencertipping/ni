# Misc invariants
```lazytest
# Some of these tests require Docker, so skip if we don't have it
if ! [[ -e /nodocker ]]; then
```

## Murmurhash probabilistic invariant
```bash
$ ni n4E7 ,hA Cubuntu[o] uc
39814375
```

The murmurhash function should have just under 1 percent error.

```lazytest
fi
```
