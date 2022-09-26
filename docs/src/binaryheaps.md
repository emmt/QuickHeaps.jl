# Binary heaps

[Binary heaps](https://en.wikipedia.org/wiki/Binary_heap) dynamically store
values in a tree structure built according to a given ordering of these values.
Thanks to this structure, a number of operations can be efficiently
implemented. For a binary heap of `n` values, pushing a new value, extracting
the least (or the greatest depending on the ordering) value out of the heap,
deleting a value, and replacing a value all have a complexity of `O(log(n))` at
worst. Just getting the least (or the greatest) value without extracting it out
of the heap is an `O(1)` operation.


## Basic usage

In `QuickHeaps`, a binary heap is created by the [`BinaryHeap`](@ref)
constructor:

```julia
h = BinaryHeap{T}(o = FastMin)
```

where `T` is the type of the values stored by the heap and `o::Ordering` is the
ordering rule for sorting values. The default `FastMin` ordering yields a
*min-heap* whose root entry is the smallest one. With `o =
ReverseOrdering(FastMin)` or `o = FastMax`, a *max-heap* is created. The root
element of a min-heap (resp. a max-heap) is the smallest one (resp. the
greatest one).

!!! warning
    Ordering `FastMin` (resp. `FastMax`) is like `Forward` (resp. `Reverse`)
    but much faster for floating-point values. However, `FastMin` and `FastMax`
    are not consistent with NaN (*Not a Number*) values. If your data may
    contains NaNs, use `Forward` or `Reverse` instead of `FastMin` or `FastMax`.
    Aliases `SafeMin=Forward` (and `SafeMax=Reverse`) are provided by the
    `QuickHeaps` package.

A vector `vals` storing the initial values of the binary heap can be specified:

```julia
h = BinaryHeap{T}(vals, o = FastMin)
```

to create a binary heap starting with the values in `vals`. Type parameter `T`
can be omitted to assume `T=eltype(vals)`. The initial values need not be
ordered, the `BinaryHeap` constructor automatically takes care of that. If
`vals` is a `Vector` instance with elements of type `T`, the binary-heap will
be directly built into `vals`. Call `BinaryHeap(copy(vals))` to create a binary
heap with its own storage.

A binary heap `h` can be used as an ordered queue of values:

```julia
pop!(h)     # yields the root value and discard it from the heap
push!(h, x) # pushes value x in heap h
```

The *root* value is the first one according to the ordering of the heap. To
examine the root value without discarding it from the heap, call either of:

```julia
peek(h)
first(h)
h[1]
```

A binary heap `h` behaves like an abstract vector (with 1-based linear
indices), in particular:

```julia
length(h)   # yields the number of values in heap h
h[i]        # yields the i-th value of heap h
h[i] = x    # sets the i-th value of heap h and heapify h
```

Note that `h[1]` is the value of the root entry of the heap `h` (the least heap
values for a min-heap, the greatest heap value for a max-heap) and that setting
a value in the heap may trigger reordering of the values stored by the heap to
maintain the binary heap structure. In particular, after doing `h[i] = x`, do
not assume that `h[i]` yields `x`.

To delete the `i`-th value from the heap `h`, call:

```julia
delete!(h, i)
```

Call `empty!(h)` to delete all the values of the binary heap `h` and
`isempty(h)` to query whether `h` is empty.

!!! note
    Operations that modify the heap, like deletion by `delete!(h,i)`,
    insertion by `h[i] = x`, pushing by `push!(h,x)`, and extracting by
    `pop!(h)` are of numerical complexity `O(1)` in the best case, `O(log(n))`
    in the worst case, with `n = length(h)` the number of values in the heap
    `h`. Query a given value with `peek(h)`, `first(h)`, or `h[i]` is always of
    complexity `O(1)`.


### Advanced usage

Instances of [`BinaryHeap`](@ref) store their values in a Julia vector whose
length is always equal to the number of stored values. Slightly faster binary
heaps are created by the [`FastBinaryHeap`](@ref) constructor. Such binary
heaps never automatically reduce the size of the array backing the storage of
their values (even though the size is automatically augmented as needed). You
may call `resize!(h)` to explicitly reduce the storage to its minimum.

A hint about the anticipated size `n` of a heap `h` (of any kind) can be set by:

```julia
sizehint!(h, n)
```

which yields `h`.


### Customize binary heaps

The behavior of the binary heap types provided by `QuickHeaps` can be tweaked
by using a particular instance of the ordering `o::Ordering` and by
specializing the `Base.lt` method called as `Base.lt(o,x,y)` to decide whether
value `x` occurs before value `y` according to ordering `o`. Note that in the
implementation of binary heaps in the `QuickHeaps` package, `x` and `y` will
always be both of type `T`, the type of the values stored by the heap.

If this is not sufficient, a custom binary heap type may be created that
inherits from `AbstractBinaryHeap{T,O}` with `T` the type of the values stored
by the heap and `O` the type of the ordering. Assuming the array backing the
storage of the values in the custom heap type has 1-based linear indexing, it
is sufficient to specialize the following methods for an instance `h` of the
custom heap type, say `CustomBinaryHeap`:

- `Base.length(h::CustomBinaryHeap)` yields the number of values in `h`;
- `Base.empty!(h::CustomBinaryHeap)` delete all values in `h`;
- [`QuickHeaps.storage`](@ref)`(h::CustomBinaryHeap)` yields the array backing
  the storage of values;
- [`QuickHeaps.ordering`](@ref)`(h::CustomBinaryHeap)`] yields the ordering of
  the values;
- [`QuickHeaps.unsafe_grow!`](@ref)`(h::CustomBinaryHeap)`
- [`QuickHeaps.unsafe_shrink!`](@ref)`(h::CustomBinaryHeap)`

to have a fully functional custom binary heap type.

By default, `Base.resize!(h)` does nothing (except returning its argument) for
any instance `h` of a type that inherits from `AbstractBinaryHeap`; but this
method may also be specialized.

The `QuickHeaps` package provides a number of methods (some unexported) that
may be useful for implementing new binary heap types:

- [`QuickHeaps.heapify`](@ref)
- [`QuickHeaps.heapify!`](@ref)
- [`QuickHeaps.isheap`](@ref)
- [`QuickHeaps.heapify_down!`](@ref)
- [`QuickHeaps.heapify_up!`](@ref)
- [`QuickHeaps.unsafe_heapify_down!`](@ref)
- [`QuickHeaps.unsafe_heapify_up!`](@ref)


Note that the `heapify`, `heapify!`, and `isheap` methods which are exported by
the `QuickHeaps` package have the same behavior but are different than those in
the [`DataStructures`](https://github.com/JuliaCollections/DataStructures.jl)
package. If you are using both packages, you'll have to explicitly prefix these
methods by the package module.


## Simple priority queues

A binary heap can be used to implement a simple [priority
queue](https://en.wikipedia.org/wiki/Priority_queue) with keys of type `K` and
values of type `V` as follows:

```julia
struct Node{K,V}
   key::K
   val::V
end
Base.lt(o::Base.Ordering, a::T, b::T) where {T<:Node} = lt(o, a.val, b.val)
Q = FastBinaryHeap{Node}()
```

This simple priority queue is a binary heap (a *min-heap* in that case) of
nodes storing key-value pairs which as sorted according to their values. The
same `Node` structure as the one defined above and with the same specialization
of `Base.lt` is provided (but not exported) by `QuickHeaps`, so a simplified
version of the above example is:

```julia
using QuickHeaps: Node
Q = FastBinaryHeap{Node}()
```

Such a priority queue is faster than `DataStructures.PriorityQueue` but it
provides no means to requeue a node nor to ensure that keys are unique. An
auxiliary array, a dictionary, or a set can be used for that, this is
implemented by [`QuickHeaps.PriorityQueue`](@ref) and
[`QuickHeaps.FastPriorityQueue`](@ref) which are more flexible and offer more
capabilities than the simple implementation in the above example.
