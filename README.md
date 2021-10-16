# Versatile binary heaps and priority queues for Julia

`QuickHeaps` is a small Julia package providing versatile binary heaps and
priority queues.  These data structures may be faster and more flexible than
those provided by
[`DataStructures`](https://github.com/JuliaCollections/DataStructures.j).

Binary heaps can be used to implement simple priority queues.  Indeed,
implementing a priority queue with keys of type `K` and values of type `V`, may
be as simple as:

```julia
struct Node{K,V}
   key::K
   val::V
end
Base.lt(o::Base.Ordering, a::Node, b::Node) = lt(o, a.val, b.val)
pq = FastBinaryHeap{Node}()
```

In words, such a priority queue is a binary heap (a min-heap in that case) of
nodes that store their value and their key and which as sorted according to
their values.  The same `Node` structure with the same specialization of
`Base.lt` is provided by `QuickHeaps`, and a simpler version of the example is:

```julia
using QuickHeaps: Node
pq = FastBinaryHeap{Node}()
```

Such a priority queue is faster than `DataStructures.PriorityQueue` but it
implements no means to requeue a node nor to ensure that keys are unique.  A
dictionary or a set can be used for that, but this would slow down the priority
queue.

A `QuickHeaps.FastBinaryHeap` does not reduce the size of the array backing the
storage of the binary heap.  This is fine when the binary heap is used as a
workspace in another algorithm.  Use `QuickHeaps.BinaryHeap` to automatically
resize the storage array so that its length is always that of the heap.


## Speed up and strengthen sorting algorithms

The sorting algorithms in Julia are very powerful but have some issues (for
me):

1. Sorting algorithms involve lots of comparisons and could be much faster if
   we know that there are no NaN's in the arrays to sort or if we assume (at
   our own risk) that they can be ignored.

2. Some non-exported methods may be very useful if they can be safely
   called.

This short note is bout dealing with these two points.


### Speed up sorting

Sorting algorithms in Julia rely on `Base.Order.lt(o,x,y)` to check whether `x`
is *before* `y` according to the ordering specified in `o` (the letters `lt`
stands for *less than*).  Most (all?) Julia sorting methods have an `order`
keyword to specify the ordering.  By default, ordering is `Base.Order.Forward`
which is the singleton of type `Base.Order.ForwardOrdering`.  Another usual
choice is to take `Base.Order.Reverse`.  In short, when sorting, you are using
the following definitions:

```julia
const Forward = ForwardOrdering()
const Reverse = ReverseOrdering()
lt(o::ForwardOrdering, a, b) = isless(a,b)
lt(o::ReverseOrdering, a, b) = lt(o.fwd,b,a)
```

So it turns out that, `isless` is eventually called not the operator `<`.  For
integers `isless(x,y)` and `x < y` are the same (at least as far as execution
time is concerned) but for floating values, `isless` takes care of NaN's which
involves overheads and this may strongly impact the execution time of sorting
algorithms.

Simple means to ignore NaN's in sorting consists in defining your own ordering
types and extend the `Base.Order.lt` method, this is pretty simple:

```julia
using Base: Ordering, ReverseOrdering
struct FastForwardOrdering <: Ordering end
const FastForward = FastForwardOrdering()
const FastReverse = ReverseOrdering(FastForward)

import Base: lt
lt(::FastForwardOrdering, a, b) = a < b
```

Then just use keyword `order=FastForward` or `order=FastReverse` in calls to
sort methods to benefit from a speed-up factor between 2 or 3.  Almost for
free!  The very same trick has been implemented in the
[`DataStructures`](https://github.com/JuliaCollections/DataStructures.jl)
package.

Checking that an array has NaN's can be checked in linear time, that is `O(n)`,
for arrays of `n` floating-point values by the following method:

```julia
function has_nans(A::AbstractArray{<:AbstractFloat})
    flag = false
    @inbounds @simd for i in eachindex(A)
        flag |= isnan(A[i])
    end
    return flag
end
```

where short-circuit has been purposely avoided to exploit SIMD optimization.
The rationale is that if you are mostly interested in arrays with no NaN's, you
expect that the entire array be checked.  A simple benchmark follows:

```julia
using BenchmarkTools
x = rand(1000);
@btime has_nans($x) # ----> 119.054 ns (0 allocations: 0 bytes)
```

which is much shorter than the time it takes to heapify the array.  This test
could be applied to arrays of floating-point values to choose between fast/slow
ordering.  This would not change the behavior of the sorting methods but would
significantly reduce the execution time most of the time.

For integer-valued arrays, it takes `O(1)` time to check for NaN's:

```julia
has_nans(A::AbstractArray{<:Integer}) = false
```

An additional speed-up by a factor between 1.5 and 2 is achievable by proper
use of `@inline`, `@inbounds` and `@propagate_inbounds` macros in the code
implementing the sorting algorithms.  This however requires to modify existing
code.


### Application to binary heaps

As an illustration of the above discussion, below is the output of a small
benchmark ran by:

```julia
julia --project test/benchmarks.jl
```

with Julia 1.6.3 on an AMD Ryzen Threadripper 2950X 16-Core Processor:

```
Timings for "DataStructures" methods (T=Float64, n=1000):
 - DataStructures.heapify!(..., Base.Forward) ---------------------->   7.225 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., Base.Reverse) ---------------------->   7.152 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., DataStructures.FasterForward()) ---->   3.183 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., DataStructures.FasterReverse()) ---->   3.231 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., QuickHeaps.FastMin) --------------->   3.208 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., QuickHeaps.FastMax) --------------->   3.232 μs (0 allocations: 0 bytes)

Timings for "QuickHeaps" methods (T=Float64, n=1000):
 - QuickHeaps.heapify!(..., Base.Forward) ------------------------->   5.540 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., Base.Reverse) ------------------------->   5.162 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., DataStructures.FasterForward()) ------->   1.652 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., DataStructures.FasterReverse()) ------->   1.697 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., QuickHeaps.FastMin) ------------------>   1.695 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., QuickHeaps.FastMax) ------------------>   1.729 μs (0 allocations: 0 bytes)

Timings for "DataStructures" methods (T=Float64, n=1000):
 - DataStructures.isheap(..., Base.Forward) ------------------------>   1.924 μs (0 allocations: 0 bytes)
 - DataStructures.isheap(..., Base.Reverse) ------------------------>   1.939 μs (0 allocations: 0 bytes)
 - DataStructures.isheap(..., DataStructures.FasterForward()) ------>   574.049 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., DataStructures.FasterReverse()) ------>   563.701 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., QuickHeaps.FastMin) ----------------->   566.147 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., QuickHeaps.FastMax) ----------------->   577.088 ns (0 allocations: 0 bytes)

Timings for "QuickHeaps" methods (T=Float64, n=1000):
 - QuickHeaps.isheap(..., Base.Forward) --------------------------->   1.845 μs (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., Base.Reverse) --------------------------->   1.847 μs (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., DataStructures.FasterForward()) --------->   386.634 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., DataStructures.FasterReverse()) --------->   399.600 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., QuickHeaps.FastMin) -------------------->   388.861 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., QuickHeaps.FastMax) -------------------->   392.095 ns (0 allocations: 0 bytes)
```

These timings show the gain in speed for `heapify!` by using `<` instead of
`isless` by a factor of 2.3 for the binary heap implemented by `DataStructures`
and by a factor of 3.2 for the binary heap implemented by `QuickHeaps`.

These timings also show that `heapify!` in `QuickHeaps` is faster than in
`DataStructures` by a factor 1.3 with standard orderings and by a factor 1.9
with faster orderings.
