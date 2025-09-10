"""

The `QuickHeaps` module implements versatile binary heaps and priority queues for Julia.

Wikipedia page https://en.wikipedia.org/wiki/Binary_heap has very clear explanations about
binary heaps.

This code was much inspired by the
[DataStructures.jl](https://github.com/JuliaCollections/DataStructures.jl) package but has a
number of improvements:

- sorting methods are almost twice faster (in part because of the total ordering but also
  because of carefully in-lining critical sections);

- methods not having the `unsafe` word in their name are safer and check their arguments for
  correctness (methods with the `unsafe` word in their name are intended to be as fast as
  possible).

"""
module QuickHeaps

export
    # From this package:
    FastMin, FastMax, TotalMin, TotalMax,
    AbstractBinaryHeap, BinaryHeap, FastBinaryHeap,
    AbstractPriorityQueue, PriorityQueue, FastPriorityQueue,
    heapify, heapify!, isheap,
    setroot!,

    # From Base (in recent versions of Julia):
    peek,

    # From DataStructures:
    enqueue!, dequeue!, dequeue_pair!

using TypeUtils: @public
@public heapify_down!
@public heapify_up!
@public unsafe_heapify_down!
@public unsafe_heapify_up!
@public unsafe_grow!
@public unsafe_shrink!
@public FastMaxOrdering
@public FastMinOrdering
@public TotalMaxOrdering
@public TotalMinOrdering
@public default_ordering
@public has_bad_values
@public has_standard_linear_indexing
@public heap_index
@public is_one_based_unit_range
@public linear_index
@public storage

# The `peek` method appeared in Julia 1.5.
@static if isdefined(Base, :peek)
    import Base: peek
end

using Base: @propagate_inbounds, OneTo, has_offset_axes, tail, front
using Base.Order: lt, Ordering

# NOTE `heapify!`, `heapify`, and `isheap` are different functions in `DataStructures`
# and in this package. Hence they are not imported from `DataStructures`.
import DataStructures: enqueue!, dequeue!, dequeue_pair!

using TypeUtils

include("types.jl")
include("utilities.jl")
include("binaryheaps.jl")
include("priorityqueues.jl")

end # module
