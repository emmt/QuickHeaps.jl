"""

The `QuickHeaps` module implements versatile binary heaps and priority queues for Julia.

Wikipedia page https://en.wikipedia.org/wiki/Binary_heap has very clear explanations about
binary heaps.

This code was much inspired by the
[DataStructures.jl](https://github.com/JuliaCollections/DataStructures.jl) package but has a
number of improvements:

- sorting methods are 2 to 4 times faster (in part because NaN are ignored but also because
  of carefully in-lining critical sections);

- methods not having the `unsafe` word in their name are safer and check their arguments for
  correctness (methods with the `unsafe` word in their name are intended to be as fast as
  possible).

"""
module QuickHeaps

export
    # Form Base.Order:
    Ordering, ForwardOrdering, ReverseOrdering, Forward, Reverse,

    # From this package:
    FastMin, FastMax, TotalMin, TotalMax,
    AbstractBinaryHeap, BinaryHeap, FastBinaryHeap,
    AbstractPriorityQueue, PriorityQueue, FastPriorityQueue,
    heapify, heapify!, isheap,
    setroot!,
    dequeue_node!,

    # From Base (in recent versions of Julia):
    peek,

    # From DataStructures:
    enqueue!, dequeue!, dequeue_pair!

using TypeUtils: @public
@public lt
@public heapify_down!
@public heapify_up!
@public unsafe_heapify_down!
@public unsafe_heapify_up!
@public unsafe_grow!
@public unsafe_shrink!
@public AbstractNode
@public Node
@public get_key
@public get_val
@public FastMaxOrdering
@public FastMinOrdering
@public TotalMaxOrdering
@public TotalMinOrdering
@public default_ordering
@public has_bad_values
@public has_standard_linear_indexing
@public heap_index
@public in_range
@public is_one_based_unit_range
@public linear_index
@public nodes
@public index
@public storage
@public ordering
@public to_key
@public to_node
@public to_val
@public typename

# The `peek` method appeared in Julia 1.5.
@static if isdefined(Base, :peek)
    import Base: peek
end

using Base: @propagate_inbounds, OneTo, has_offset_axes
using Base.Order: Ordering

import DataStructures:
    # heapify!, heapify, isheap
    enqueue!, dequeue!, dequeue_pair!

using TypeUtils

include("types.jl")
include("utilities.jl")
include("binaryheaps.jl")
include("nodes.jl")
include("priorityqueues.jl")

end # module
