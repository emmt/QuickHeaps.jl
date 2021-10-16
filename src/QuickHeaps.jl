"""

The `QuickHeaps` module implements versatile binary heaps and priority queues
for Julia.

Wikipedia page https://en.wikipedia.org/wiki/Binary_heap has very clear
explanations about binary heaps.

This code was much inspired by the DataStructures.jl package (see
https://github.com/JuliaCollections/DataStructures.jl) but has a number of
improvements:

- sorting methods are 2 to 4 times faster (in part because NaN are
  ignored but also because of carefully in-lining critical sections);

- methods not having the `unsafe` word in their name are safer and check
  their arguments for correctness (methods with the `unsafe` word in
  their name are in tended to be as fast as possible).

"""
module QuickHeaps

export
    #FastMin, FastMax,
    #SafeMin, SafeMax,
    FastBinaryHeap,
    BinaryHeap,
    FastPriorityQueue,
    PriorityQueue,
    peek,
    enqueue!,
    dequeue!

import Base:
    IndexStyle,
    delete!,
    empty!,
    first,
    getindex,
    isempty,
    length,
    peek,
    pop!,
    push!,
    resize!,
    setindex!,
    show,
    size,
    sizehint!

# The `peek` method appeared in Julia 1.5.
@static if isdefined(Base, :peek)
    import Base: peek
end

using Base: @propagate_inbounds, OneTo, has_offset_axes

#------------------------------------------------------------------------------
# In order to perform fast sorting (without taking care of NaN's), we
# extend `Base.Order.lt` method for specialized ordering types.  The
# ordering can be an instance or its type.

using Base.Order: Ordering, ReverseOrdering, Forward, Reverse
import Base.Order: lt

"""
    FastMinOrdering

is the singleton type for fast *forward* ordering without considering NaN's.

"""
struct FastMinOrdering <: Ordering end

lt(::FastMinOrdering, a, b) = a < b

const FastMin = FastMinOrdering()
const FastMax = ReverseOrdering(FastMin)

const SafeMin = Forward
const SafeMax = Reverse

# Same default ordering as algorithms in base Julia and in DataStructures.
const DefaultOrdering = Forward

#------------------------------------------------------------------------------

include("utilities.jl")
include("binaryheaps.jl")
include("priorityqueues.jl")

end # module
