# Intent and ni economics
ni operates within a fairly open-ended space; it can move computation between
machines, distribute things, and is aware of economic forces like resource
scarcity and metered connections. Its goal is to minimize the cost associated
with executing the thing you've asked it to do, which in some cases means not
doing it at all.

## Optimization function
ni uses _win_ as a unit of success, and _fail_ = -_win_ to measure cost. When
you run a command, you assign an amount of win to the successful execution of
the command; ni is then responsible for finding a solution that is a net win,
as opposed to a net fail. (Put differently, execution strategies are budgeted
against the value of requests.)

Win and fail are formal, linear units of measure that are defined as monotonic
composites of individual externalities. The most obvious externality is time,
but others are currency, disk usage, SSD writes, power consumption, CPU usage
on a battery-powered device, and user attention.

## Risk and nonlinearity
Disk usage is an unusual externality because the marginal cost of 1GB depends
strongly on whether the free space is, e.g. 80GB, or whether it's 1.5GB. A more
correct way to measure disk usage is to express it as the _risk that the disk
will become full_, given a probability distribution of Δ(disk usage)/Δt and the
amount of free space.

A formalization for this might be `disk space == 0 = 1kf`; then ni can trade
time for risk-adjusted disk usage (via compression) to minimize expected fail.

## Strategy calculation
ni's strategy calculation is treated as a process subject to the same win/fail
calculation as commands you run: if the fail gradient against time is too
steep, ni will use a simple local strategy calculation -- but if it's a
long-running thing and the dominant gradient is nontemporal (e.g. "don't
require human intervention"), ni might use networked machines to search the
solution space for some amount of time, possibly in parallel with executing an
obvious solution.

ni never forgets things. Each solution is used to refine an internal ML-based
predictor of "the chosen solution"; this will cause ni to converge to fast and
good solutions to frequently-asked questions.
