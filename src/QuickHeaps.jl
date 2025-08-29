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
    FastForward, FastReverse,
    FastMin, FastMax, SafeMin, SafeMax,
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
@public FastForwardOrdering
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
@public to_eltype
@public to_key
@public to_node
@public to_val
@public typename

import Base:
    IndexStyle,
    IteratorEltype,
    IteratorSize,
    Pair,
    Tuple,
    copy,
    delete!,
    eltype,
    empty!,
    first,
    get,
    getindex,
    getkey,
    haskey,
    isempty,
    iterate,
    keytype,
    keys,
    length,
    peek,
    pop!,
    push!,
    resize!,
    setindex!,
    show,
    size,
    sizehint!,
    valtype,
    values

# The `peek` method appeared in Julia 1.5.
@static if isdefined(Base, :peek)
    import Base: peek
end

using Base: @propagate_inbounds, OneTo, has_offset_axes, HasEltype, HasLength

import DataStructures:
    # heapify!, heapify, isheap
    enqueue!, dequeue!, dequeue_pair!

using TypeUtils

@deprecate to_eltype(A, x) as(eltype(A), x) false

#-------------------------------------------------------------------------------------------
# In order to perform fast sorting (without taking care of NaN's), we extend `Base.Order.lt`
# method for specialized ordering types. The ordering can be an instance or its type.

using Base.Order: Ordering, ForwardOrdering, ReverseOrdering, Forward, Reverse
import Base.Order: lt

"""
    FastForwardOrdering

is the singleton type for fast *forward* ordering without considering NaN's.

"""
struct FastForwardOrdering <: Ordering end

lt(::FastForwardOrdering, a, b) = a < b

const FastForward = FastForwardOrdering()
const FastReverse = ReverseOrdering(FastForward)

const FastMin = FastForward
const FastMax = FastReverse

const SafeMin = Forward
const SafeMax = Reverse

"""
    default_ordering(T)

yields the default ordering for an ordered data structure of type `T`. This method shall be
specialized for each ordered data structure.

"""
default_ordering(x::Any) = default_ordering(typeof(x))
default_ordering(::Type) = Forward

#-------------------------------------------------------------------------------------------

include("utilities.jl")
include("binaryheaps.jl")
include("nodes.jl")
include("priorityqueues.jl")

end # module
