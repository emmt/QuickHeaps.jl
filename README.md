# Versatile binary heaps and priority queues for Julia

`QuickHeaps` is a small [Julia][julia-url] package providing versatile [binary
heaps](#binary-heaps) and [priority queues](#priority-queues).  These data
structures are more flexible and may be quite significantly faster than those
provided by [`DataStructures`][datastructures-url].

| **License**                     | **Build Status**                  | **Code Coverage**                                                   |
|:--------------------------------|:----------------------------------|:--------------------------------------------------------------------|
| [![][license-img]][license-url] | [![][appveyor-img]][appveyor-url] | [![][coveralls-img]][coveralls-url] [![][codecov-img]][codecov-url] |


## Binary heaps

[Binary heaps](https://en.wikipedia.org/wiki/Binary_heap) dynamically store
values in a tree structure built according to a given ordering of these values.
Thanks to this structure, a number of operations can be implemented so as to be
very efficient.  If a binary heap contains `n` values, pushing a new value,
extracting the least (or the greatest) value, deleting a value, and replacing a
value all have a complexity of `O(log(n))` at worst.  Just getting the least
(or the greatest) value without extracting it from the heap is an `O(1)`
operation.


### Basic usage

A binary heap is created by:

```julia
h = BinaryHeap{T}(o = FastMin)
```

where `T` is the type of values stored in the heap and `o::Ordering` is the
ordering to use for sorting values.  By default, `FastMin` ordering is used
which yields a min-heap.  With `o = ReverseOrdering(FastMin)` or `o = FastMax`,
a max-heap is created.

:warning: Ordering `FastMin` (resp. `FastMax`) is like `Forward`
(resp. `Reverse`) but much faster for floating-point values.  However,
`FastMin` and `FastMax` are not consistent with NaN (*Not a Number*) values.
If your data may contains NaNs, use `Forward` or `Reverse` instead of `FastMin`
or `FastMax`.  Aliases `SafeMin=Forward` (and `SafeMax=Reverse`) are provided
by the `QuickHeaps` package.

A vector `vals` storing the initial values of the binary heap can be specified:

```julia
h = BinaryHeap{T}(vals, o = FastMin)
```

to create a binary heap starting with the values in `vals`.  Type parameter `T`
can be omitted to assume `T=eltype(vals)`.  The initial values need not be
ordered, the `BinaryHeap` constructor automatically takes care of that.  If
`vals` is a `Vector` instance with elements of type `T`, the binary-heap will
be directly built into `vals`.  Call `BinaryHeap(copy(vals))` to create a
binary heap with its own storage.

A binary heap `h` can be used as an ordered queue of values:

```julia
pop!(h)     # yields the first value and discard its node
push!(h, x) # pushes value x in heap h
```

The *first value* is the first one according to the ordering of the heap.
To examine the first value without discarding its node, call either of:

```julia
peek(h)
first(h)
h[1]
```

A binary heap `h` behaves like an abstract vector (with 1-based linear
indices), in particular:

```julia
length(h)   # yields the number of nodes in heap h
h[i]        # yields the value of the i-th node of heap h
h[i] = x    # sets the value of the i-th node of heap h and heapify h
```

Note that `h[1]` is the value of the root node of the heap `h` and that setting
the value of a node in the heap may trigger reordering of the nodes to maintain
the binary heap structure.  In other words, after doing `h[i] = x`, do not
assume that `h[i]` yields `x`.

To delete the `i`-th node from the heap `h`, call:

```julia
delete!(h, i)
```

Call `empty!(h)` to delete all the nodes of the binary heap `h` and `isempty(h)`
to query whether `h` is empty.

:heart: Operations that modify the heap, like deletion by `delete!(h,i)`,
insertion by `h[i] = x`, pushing by `push!(h,x)`, and extracting by `pop!(h)`
are of complexity `O(1)` in the best case, `O(log(n))` in the worst case, with
`n = length(h)` the number of nodes in the heap `h`.  Query the value of a
given node by `peek(h)`, `first(h)`, or `h[i]` is always of complexity `O(1)`.


### Advanced usage

Instances of `BinaryHeap` store their values in a Julia vector whose length is
always equal to the number of stored values.  Slightly faster binary heaps are
created by the `FastBinaryHeap` constructor.  Such binary heaps never
automatically reduce the size of the array backing the storage of their values
(even though the size is automatically augmented as needed).  You may call
`resize!(h)` to explicitly reduce the storage to its minimum.

A hint about the anticipated size `n` of a heap `h` (of any kind) can be set by:

```julia
sizehint!(h, n)
```

which yields `h`.


### Customize binary heaps

The behavior of the binary heap types provided by `QuickHeaps` can be tweaked
by using a particular instance of the ordering `o::Ordering` and by
specializing the `Base.lt` method called as `Base.lt(o,x,y)` to decide whether
value `x` occurs before value `y` according to ordering `o`.  Note that in the
implementation of binary heaps in the `QuickHeaps` package, `x` and `y` will
always be both of type `T`, the type of the values stored by the heap.

If this is not sufficient, a custom binary heap type may be created that
inherits from `AbstractBinaryHeap{T,O}` with `T` the type of the values stored
by the heap and `O` the type of the ordering.  Assuming the array backing the
storage of the values in the custom heap type has 1-based linear indexing, it
is sufficient to specialize the following methods for an instance `h` of the
custom heap type:

```julia
length(h)
empty!(h)
QuickHeaps.nodes(h)
QuickHeaps.ordering(h)
QuickHeaps.unsafe_grow!(h)
QuickHeaps.unsafe_shrink!(h)
```

to have a fully functional custom binary heap type.

By default, `resize!(h)` does nothing (except returning its argument) for any
instance `h` of a type that inherits from `AbstractBinaryHeap`; but this method
may also be specialized.

The `QuickHeaps` package provides a number of methods (some unexported) that
may be useful for implementing new binary heap types:

```julia
heapify
heapify!
isheap
QuickHeaps.heapify_down!
QuickHeaps.heapify_up!
QuickHeaps.unsafe_heapify_down!
QuickHeaps.unsafe_heapify_up!
```

Note that the `heapify`, `heapify!`, and `isheap` methods which are exported by
the `QuickHeaps` package have the same behavior but are different than those in
the [`DataStructures`][datastructures-url] package.  If you are using both
packages, you'll have to explicitly prefix these methods by the package module.


## Priority queues

Binary heaps can be used to implement simple priority queues.  Indeed,
implementing a priority queue, with keys of type `K` and values of type `V`, may
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
their values.  The above `Node` structure with the same specialization of
`Base.lt` is provided (but not exported) by `QuickHeaps`, and a simpler version
of the example is:

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

So it turns out that, `isless` is eventually called, not the operator `<`.  For
integers `isless(x,y)` and `x < y` are the same (at least as far as execution
time is concerned) but for floating-point values, `isless` takes care of NaN's
which involves overheads and this may strongly impact the execution time of
sorting algorithms.

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
sort methods to benefit from a speed-up factor between 2 or 3.  Almost for free!
The very same trick has been implemented in the
[`DataStructures`](https://github.com/JuliaCollections/DataStructures.jl)
package with `DataStructures.FasterForward()` and
`DataStructures.FasterReverse()`.

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

An additional speed-up by a factor between 1.5 and 2 is achievable by proper use
of `@inline`, `@inbounds` and `@propagate_inbounds` macros in the code
implementing the sorting algorithms.  This however requires to modify existing
code.  This is what is done in
[`QuickHeaps`](https://github.com/emmt/QuickHeaps.jl).


### Application to binary heaps

As an illustration of the above discussion, below is the output of a small
benchmark ran by:

```julia
julia --project test/benchmarks.jl
```

with Julia 1.6.3 on an AMD Ryzen Threadripper 2950X 16-Core Processor:

```
Timings for "DataStructures" methods (T=Float64, n=1000):
 - DataStructures.heapify!(..., Base.Forward) ---------------------> 7.478 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., Base.Reverse) ---------------------> 7.268 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., DataStructures.FasterForward()) ---> 3.444 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., DataStructures.FasterReverse()) ---> 3.428 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., QuickHeaps.FastMin) ---------------> 3.413 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., QuickHeaps.FastMax) ---------------> 3.428 μs (0 allocations: 0 bytes)

Timings for "QuickHeaps" methods (T=Float64, n=1000):
 - QuickHeaps.heapify!(..., Base.Forward) -------------------------> 4.852 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., Base.Reverse) -------------------------> 4.506 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., DataStructures.FasterForward()) -------> 1.655 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., DataStructures.FasterReverse()) -------> 1.658 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., QuickHeaps.FastMin) -------------------> 1.637 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., QuickHeaps.FastMax) -------------------> 1.658 μs (0 allocations: 0 bytes)

Timings for "DataStructures" methods (T=Float64, n=1000):
 - DataStructures.isheap(..., Base.Forward) -----------------------> 1.910 μs (0 allocations: 0 bytes)
 - DataStructures.isheap(..., Base.Reverse) -----------------------> 1.932 μs (0 allocations: 0 bytes)
 - DataStructures.isheap(..., DataStructures.FasterForward()) -----> 563.027 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., DataStructures.FasterReverse()) -----> 575.110 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., QuickHeaps.FastMin) -----------------> 575.087 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., QuickHeaps.FastMax) -----------------> 573.750 ns (0 allocations: 0 bytes)

Timings for "QuickHeaps" methods (T=Float64, n=1000):
 - QuickHeaps.isheap(..., Base.Forward) ---------------------------> 1.820 μs (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., Base.Reverse) ---------------------------> 1.821 μs (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., DataStructures.FasterForward()) ---------> 381.527 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., DataStructures.FasterReverse()) ---------> 383.847 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., QuickHeaps.FastMin) ---------------------> 378.627 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., QuickHeaps.FastMax) ---------------------> 384.631 ns (0 allocations: 0 bytes)
```

These timings show the gain in speed for `heapify!` by using `<` instead of
`isless` by a factor of 2.3 for the binary heap implemented by `DataStructures`
and by a factor of 3.2 for the binary heap implemented by `QuickHeaps`.

These timings also show that `heapify!` in `QuickHeaps` is faster than in
`DataStructures` by a factor greater than 1.5 with standard orderings and by a
factor better than 2 with faster orderings.


[julia-url]: https://julialang.org/
[datastructures-url]: https://github.com/JuliaCollections/DataStructures.jl

[license-url]: ./LICENSE.md
[license-img]: http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat

[travis-img]: https://travis-ci.com/emmt/QuickHeaps.jl.svg?branch=master
[travis-url]: https://travis-ci.com/emmt/QuickHeaps.jl

[appveyor-img]: https://ci.appveyor.com/api/projects/status/github/emmt/QuickHeaps.jl?branch=master
[appveyor-url]: https://ci.appveyor.com/project/emmt/QuickHeaps-jl/branch/master

[coveralls-img]: https://coveralls.io/repos/emmt/QuickHeaps.jl/badge.svg?branch=master&service=github
[coveralls-url]: https://coveralls.io/github/emmt/QuickHeaps.jl?branch=master

[codecov-img]: http://codecov.io/github/emmt/QuickHeaps.jl/coverage.svg?branch=master
[codecov-url]: http://codecov.io/github/emmt/QuickHeaps.jl?branch=master
