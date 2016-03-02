# Pipe speed (per-process throughput overhead)
I ran `./pipe-speed` on a machine with four CPUs:

```sh
$ cat /proc/cpuinfo
...
processor	: 3
vendor_id	: GenuineIntel
cpu family	: 6
model		: 61
model name	: Intel(R) Core(TM) i5-5200U CPU @ 2.20GHz
stepping	: 4
microcode	: 0x1d
cpu MHz		: 2536.015
cache size	: 3072 KB
physical id	: 0
siblings	: 4
core id		: 1
cpu cores	: 2
apicid		: 3
initial apicid	: 3
fpu		: yes
fpu_exception	: yes
cpuid level	: 20
wp		: yes
flags		: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm pbe syscall nx pdpe1gb rdtscp lm constant_tsc arch_perfmon pebs bts rep_good nopl xtopology nonstop_tsc aperfmperf eagerfpu pni pclmulqdq dtes64 monitor ds_cpl vmx est tm2 ssse3 fma cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic movbe popcnt tsc_deadline_timer aes xsave avx f16c rdrand lahf_lm abm 3dnowprefetch ida arat epb pln pts dtherm intel_pt tpr_shadow vnmi flexpriority ept vpid fsgsbase tsc_adjust bmi1 avx2 smep bmi2 erms invpcid rdseed adx smap xsaveopt
bugs		:
bogomips	: 4389.49
clflush size	: 64
cache_alignment	: 64
address sizes	: 39 bits physical, 48 bits virtual
power management:
$
```

One cool thing about this experiment is that we're measuring almost exactly what
we want to be: `/bin/cat` easily fits into the processor cache, and every
process is identical (which I think means they're mapped into the same physical
pages).

Linux also uses multiple processors to handle the data forwarding. In this case
most of the time is spent in `sys`, not `user`, indicating that the kernel
itself is the bottleneck. That's about what we'd expect, since it's copying the
memory and `cat` just makes system calls without touching the data itself.

The output is in `pipe-speed.log`; taking the minimum for each chain length
(these numbers are `real` times):

```sh
$ nfu pipe-speed.log --fold '%0 =~ /^$/../^sys/' \
      -f03m 'row %0, $1 * 60 + $2 if %1 =~ /(\d+)m([0-9.]+)s/' \
      -A 'row $_, min @{%1}'
0       0.141
1       0.455
2       0.671
4       1.204
8       2.768
16      6.215
32      12.527
64      25.405
128     52.642
256     110.066
512     216.976
1024    405.044
```

Solidly linear, which is great. So we can push 1.05GB of data through 1024
processes in 409.044 seconds: a total kernel+user throughput of 2.629GB/sec to
move data between two processes' user memory. This basic rate remains true
across the whole range, give or take about 10% (the single-length chain, with
two processes, performs a little worse at 2.308GB/sec).
