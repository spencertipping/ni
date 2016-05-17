# Grouping operators
Implementing `g` with `sort` works, but might be overkill given that nothing
about most groupings demands that the data emerge sorted. The only relevant
requirement is that all like records end up next to each other.

One advantage of a non-sorting group operator is that it can begin emitting
output before all input has been received. It obviously needs to buffer
everything before the second group comes out, but the first group can happen
immediately -- which has the potential to improve iteration performance
considerably in certain cases. (Actually, I'm not sure it does: if you're
doing the usual group+aggregate, then the aggregation itself will block until
it sees either EOF or a distinct value. So the only benefit is that we don't
have to wait for a full sort before seeing the first piece of data, but I'm
not sure how compelling that is.)
