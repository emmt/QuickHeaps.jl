# Type and constant definitions.

using Base.Order: Ordering, ForwardOrdering, ReverseOrdering, Forward, Reverse

"""
    FastForwardOrdering

is the singleton type for fast *forward* ordering without considering NaN's.

"""
struct FastForwardOrdering <: Ordering end

const FastForward = FastForwardOrdering()
const FastReverse = ReverseOrdering(FastForward)

const FastMin = FastForward
const FastMax = FastReverse

const SafeMin = Forward
const SafeMax = Reverse

"""
    QuickHeaps.AbstractNode{K,V}

is the super-type of nodes with a key of type `K` and a value of type `V`. Nodes can be used
in binary heaps and priority queues to represent key-value pairs and specific ordering rules
may be imposed by specializing the `QuickHeaps.lt` function which, for abstract nodes, is by
default:

    QuickHeaps.lt(o::Ordering, x::T, y::T) where {T<:QuickHeaps.AbstractNode} =
        Base.Order.lt(o, QuickHeaps.get_val(x), QuickHeaps.get_val(y))

"""
abstract type AbstractNode{K,V} end

struct Node{K,V} <: AbstractNode{K,V}
    key::K
    val::V
    Node{K,V}(key, val) where {K,V} = new{K,V}(key, val)
end

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

is the super type of priority queues with ordering of type `O<:Base.Ordering` and storing
nodes associating a key of type `K` with a priority value of type `V`.

Package `QuickHeaps` provides two concrete types of priority queues: [`PriorityQueue`](@ref)
for any kind of keys and [`FastPriorityQueue`](@ref) for which keys are analogous to array
indices.

Priority queues behave like dictionaries with the additional feature of automatically
maintaining an ordered structure according to the priority queue ordering and the node
values. For a priority queue `pq`, retrieving the *root* node (i.e., the one of highest
priority) without removing it costs `O(1)` and is done by:

    peek(pq, T=Pair) -> T(key, val)

with `T` the type of the expected result. Retrieving the value of a node given its `key` has
also an `O(1)` complexity and is done by one of:

    pq[key...] -> val
    getindex(pq, key...) -> val
    get(pq, key, def) -> val_at_key_or_def

Changing the content of the priority queue has a complexity of `O(log(n))` with `n =
length(pq)` the number of nodes in the queue. This includes removing the node at `key`
by:

    delete!(pq, key) -> pq

or removing the root node by:

    pop!(pq)          # -> root node as a `key=>val` pair
    dequeue!(pq)      # -> key of root node
    dequeue_pair!(pq) # -> root node as a `key=>val` pair
    dequeue_node!(pq) # -> root node as stored in priority queue

or setting a node `x` with a given `key` and value `val` by one of:

    pq[key] = val
    enqueue!(pq, key, val)
    enqueue!(pq, key => val)
    push!(pq, key => val)
    push!(pq, (key, val))
    push!(pq, x) # x is a node with a key and a value

"""
abstract type AbstractPriorityQueue{K,V,O<:Ordering} <: AbstractDict{K,V} end

struct PriorityQueue{K,V,O,T} <: AbstractPriorityQueue{K,V,O}
    order::O
    nodes::Vector{T}
    index::Dict{K,Int}
end

struct FastPriorityQueue{V,N,O,
                         T<:AbstractNode{Int,V}} <: AbstractPriorityQueue{Int,V,O}
    order::O
    nodes::Vector{T}
    index::Array{Int,N}
end
