# Type and constant definitions.

struct TotalMinOrdering <: Ordering end
struct TotalMaxOrdering <: Ordering end
struct FastMinOrdering <: Ordering end

"""
    TotalMin

Singleton for *total min ordering* considering NaN's as greater than any other
floating-point value, and `missing` to be greater than anything else. With this ordering,
values are sorted in ascending order, followed by `NaN` then `missing` values.

`TotalMin` is similar to the default ordering in most Julia algorithms and which is
implemented by `isless`. However, for arrays of floating-point values with 50% of NaN's,
`TotalMin` is nearly as fast as (19%) using `<` and is almost twice faster (82%) than
`isless`. This speed-up is obtained by using non-branching bitwise operators instead of
logical operators.

`QuickHeaps.TotalMinOrdering` is the type of `TotalMin`.

Also see [`TotalMax`](@ref).

"""
const TotalMin = TotalMinOrdering()
@doc @doc(TotalMin) TotalMinOrdering

"""
    TotalMax

Singleton for *total max ordering*. With this ordering, values are sorted in decreasing
order, followed by `NaN` then `missing` values.

`QuickHeaps.TotalMaxOrdering` is the type of `TotalMax`.

!!! note
    `TotalMax` and `reverse(TotalMin)` both sort regular values in decreasing order but the
    latter puts `missing` then `NaN` values *first*.

Also see [`TotalMin`](@ref).

"""
const TotalMax = TotalMaxOrdering()
@doc @doc(TotalMax) TotalMaxOrdering

"""
    QuickHeaps.FastMinOrdering <: Base.Order.Ordering
    const FastMin = QuickHeaps.FastMinOrdering()

Singleton for *min ordering*. This ordering is faster than [`TotalMin`](@ref) but leaves
indefinite the order of `NaN` values.

`QuickHeaps.FastMinOrdering` is the type of `FastMin`.

Also see [`FastMax`](@ref).

"""
const FastMin = FastMinOrdering()
@doc @doc(FastMin) FastMinOrdering

"""
    FastMax

Singleton for *max ordering*. This ordering is faster than [`TotalMax`](@ref) but leaves
indefinite the order of `NaN` values.

`QuickHeaps.FastMaxOrdering` is an alias to the type of `FastMax`.

Also see [`FastMin`](@ref).

"""
const FastMax = Base.Order.ReverseOrdering(FastMin)
const FastMaxOrdering = typeof(FastMax)
@doc @doc(FastMax) FastMaxOrdering

"""
    QuickHeaps.AbstractBinaryHeap{T,O}

is the super-type of binary heaps in `QuickHeaps` whose values have type `T` and whose
ordering has type `O`.

The following methods are available for a binary heap `h` (those which modify the heap
contents re-order heap values as needed to maintain the heap structure):

    pop!(h)        # deletes and returns root value of heap h
    push!(h, x)    # pushes value x in heap h
    empty!(h)      # empties heap h
    isempty(h)     # yields whether heap h is empty
    delete!(h, i)  # deletes i-th value from heap h
    peek(h)        # yields root value of heap h without deleting it
    first(h)       # idem
    setroot!(h, x) # same as h[1] = x, replaces root value of heap h by x

A binary heap `h` behaves like an abstract vector (with 1-based linear indices), in
particular:

    length(h)   # the number of values in heap h
    h[i]        # the i-th value of heap h
    h[i] = x    # set the i-th value of heap h and heapify h

Note that `h[1]` is the root value of the heap `h` and that setting a value in the heap may
trigger reordering of the values to maintain the binary heap structure. In other words,
after doing `h[i] = x`, do not assume that `h[i]` yields `x`.

Operations that modify the heap, like deletion by `delete!(h,i)`, insertion by `h[i] = x`,
pushing by `push!(h,x)`, and extracting by `pop!(h)` are of complexity `O(1)` in the best
case, `O(log(n))` in the worst case, with `n = length(h)` the number of values in the heap
`h`. Retrieving a given value with `peek(h)`, `first(h)`, or `h[i]` is always of complexity
`O(1)`.

Call `Base.Order.Ordering(h)` to retrieve the ordering object `o` for the binary heap `h`.

"""
abstract type AbstractBinaryHeap{T,O<:Ordering} <: AbstractVector{T} end

struct BinaryHeap{T,O} <: AbstractBinaryHeap{T,O}
    order::O        # ordering of values
    data::Vector{T} # storage for the values
    BinaryHeap{T}(o::O=default_ordering(BinaryHeap)) where {T,O<:Ordering} =
        new{T,O}(o, Vector{T}(undef, 0))
    BinaryHeap{T}(o::O, vals::AbstractVector) where {T,O<:Ordering} =
        heapify!(new{T,O}(o, vals))
end

mutable struct FastBinaryHeap{T,O} <: AbstractBinaryHeap{T,O}
    order::O        # ordering of values
    data::Vector{T} # storage for the values
    count::Int      # current number of values
    FastBinaryHeap{T}(o::O=default_ordering(FastBinaryHeap)) where {T,O<:Ordering} =
        new{T,O}(o, Vector{T}(undef, 0), 0)
    FastBinaryHeap{T}(o::O, vals::AbstractVector) where {T,O<:Ordering} =
        heapify!(new{T,O}(o, vals, length(vals)))
end

"""
    QuickHeaps.AbstractPriorityQueue{K,V,O}

is the super type of priority queues with ordering of type `O<:Base.Order.Ordering` and
storing keys of type `K` associated priority values of type `V`.

Package `QuickHeaps` provides two concrete types of priority queues: [`PriorityQueue`](@ref)
for any kind of keys and [`FastPriorityQueue`](@ref) for which keys are analogous to array
indices.

Priority queues behave like dictionaries with the additional feature of automatically
maintaining an ordered structure according to the priority queue ordering and the entry
values. For a priority queue `pq`, retrieving the *root* entry, that is the pair `key =>
val` of highest priority, without removing it costs `O(1)` and is done by:

    peek(pq) -> (key => val)

Retrieving the value of an entry given its `key` has also an `O(1)` complexity and is done
by one of:

    pq[key...] -> val
    getindex(pq, key...) -> val
    get(pq, key, def) -> val_at_key_or_def

Changing the content of the priority queue has a complexity of `O(log(n))` with `n =
length(pq)` the number of queued entries. This includes removing the entry at `key` by:

    delete!(pq, key) -> pq

removing the root entry by:

    pop!(pq)          # -> root entry as a `key=>val` pair
    dequeue!(pq)      # -> key of root entry
    dequeue_pair!(pq) # -> root entry as a `key=>val` pair

or setting/changing an entry with a given `key` and value `val` by one of:

    pq[key] = val
    enqueue!(pq, key => val)
    push!(pq, key => val)

Call `Base.Order.Ordering(pq)` to retrieve the ordering object `o` for the priority queue
`pq`.

"""
abstract type AbstractPriorityQueue{K,V,O<:Ordering} <: AbstractDict{K,V} end

struct PriorityQueue{K,V,O} <: AbstractPriorityQueue{K,V,O}
    order::O
    pairs::Vector{Pair{K,V}} # heap of entries
    index::Dict{K,Int}       # key to heap index mapping
end

struct FastPriorityQueue{V,N,O} <: AbstractPriorityQueue{Int,V,O}
    order::O
    pairs::Vector{Pair{Int,V}} # heap of entries
    index::Array{Int,N}        # key to heap index mapping

    # The following private inner constructor yields an object that may alias its arguments.
    global _FastPriorityQueue
    function _FastPriorityQueue(order::O,
                                pairs::AbstractVector{Pair{Int,V}},
                                index::AbstractArray{Int,N}) where {V,N,O}
        N ≥ 1 || throw(ArgumentError("number of dimensions must be at least 1"))
        return new{V,N,O}(order, pairs, index)
    end
end
