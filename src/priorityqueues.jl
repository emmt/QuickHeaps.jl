"""
    QuickHeaps.AbstractNode{K,V}

is the super-type of nodes with a key of type `K` and a value of type `V`.
Nodes can be used in binary heaps and priority queues where they are used to
store key-value pairs and, by specializing the `Base.lt` method, to implement
how the nodes are ordered.

A node `nd` can be iterated and converted into a pair or a 2-tuple and
conversely:

    k, v = nd               # k is the key and v is the value of the node nd
    Pair(nd)                # yields k=>v
    Tuple(nd)               # yields (k,v)
    QuickHeaps.Node((k, v)) # is the same as QuickHeaps.Node(k, v)
    QuickHeaps.Node(k => v) # is the same as QuickHeaps.Node(k, v)

The `getkey` and (unexported) [`QuickHeaps.getval`](@ref) methods can be used
to respectively retrieve the key and value of a node.  These two methods may be
specialized for the sub-types of [`QuickHeaps.AbstractNode`](@ref).  For
example, a *key-only* node type can be fully implemented by:

    struct KeyOnlyNode{K} <: QuickHeaps.AbstractNode{K,Nothing}
        key::K
    end
    QuickHeaps.getkey(x::KeyOnlyNode) = getfield(x, :key)
    QuickHeaps.getval(x::KeyOnlyNode) = nothing
    KeyOnlyNode(x::Tuple{K,Nothing}) where {K} = KeyOnlyNode{K}(x[1])
    KeyOnlyNode(x::Pair{K,Nothing}) where {K} = KeyOnlyNode{K}(x.first)

"""
abstract type AbstractNode{K,V} end

"""
    QuickHeaps.AbstractPriorityQueue{K,V,T,O}

is the super type of priority queues with nodes consisting in pairs of keys of
type `K` and priority values of type `V`.  Priority queues implement an API
similar to dictionaries with the additional feature of maintaining an ordered
structure so that getting the node of highest priority costs `O(1)` while
pushing a node costs `O(log(n))` with `n` the size of the queue.

Type parameter `T<:AbstractNode{K,V}` is the type of the nodes stored in the
queue and type parameter `O` is the type of the ordering of the queue.  How are
ordered the nodes is completely customizable by specializing the `Base.lt`
method with the following signature:

    lt(o::CustomOrderingType, a::CustomNodeType, b::CustomNodeType)

which shall yield whether node `a` has (strictly) higher priority than node `b`
in the queue and where `CustomOrderingType<:Base.Ordering` and
`CustomNodeType<:QuickHeaps.AbstractNode` are the respective types of the
ordering and of the node of the priority queue.

For the default node type, `QuickHeaps.Node{K,V}`, the implementation is:

    lt(o::Ordering, a::T, b::T) where {T<:Node} = lt(o, a.val, b.val)

In other words, nodes are sorted by their value according to ordering `o`.

"""
abstract type AbstractPriorityQueue{
    K,V,T<:AbstractNode{K,V},O<:Ordering} <: AbstractDict{K,V} end

typename(::Type{<:AbstractPriorityQueue}) = "priority queue"

"""
    QuickHeaps.Node{K,V}(k,v)

yields a node storing key `k` and value `v`.  Type parameters `K` and `V` are
the respective types of the key and of the value.  If omitted the defaults are
`K=typeof(k)` and `V=typeof(v)`.

""" Node

# The following structure was introduced to be more specific than Pair{K,V}.
# So Base.lt can be specialized without type-piracy.
struct Node{K,V} <: AbstractNode{K,V}
    key::K
    val::V
end

# These methods can be specialized.
import Base: getkey

"""
    getkey(x::QuickHeaps.AbstractNode) -> k

yields the key `k` of node `x`.  This method may be specialized for any
sub-types of [`QuickHeaps.AbstractNode`](@ref).

Also see [`QuickHeaps.getval`](@ref).

"""
getkey(x::Node) = getfield(x, :key)

"""
    QuickHeaps.getval(x::QuickHeaps.AbstractNode) -> v

yields the value `v` of node `x`.  This method may be specialized for any
sub-types of [`QuickHeaps.AbstractNode`](@ref).

Also see [`getkey(::QuickHeaps.AbstractNode)`](@ref).

"""
getval(x::Node) = getfield(x, :val)

for type in (:AbstractNode, :Node)
    @eval begin
        $type(x::$type) = x
        $type{K}(x::$type{K}) where {K} = x
        $type{K,V}(x::$type{K,V}) where {K,V} = x
    end
end
Node(x::AbstractNode) = Node(getkey(x), keyval(x))
Node{K}(x::AbstractNode) where {K} = Node{K}(getkey(x), keyval(x))
Node{K,V}(x::AbstractNode{K,V}) where {K,V} = Node{K,V}(getkey(x), keyval(x))

Node(x::Tuple{Any,Any}) = Node(x[1], x[2])
Tuple(x::AbstractNode) = (getkey(x), getval(x))

Node(x::Pair) = Node(x.first, x.second)
Pair(x::AbstractNode) = getkey(x) => getval(x)

iterate(x::AbstractNode) = (getkey(x), first)
iterate(x::AbstractNode, ::typeof(first)) = (getval(x), last)
iterate(x::AbstractNode, ::typeof(last)) = nothing

# Nodes are sorted according to their values.
for O in (:Ordering, :ForwardOrdering, :ReverseOrdering, :FastForwardOrdering)
    @eval begin
        lt(o::$O, a::T, b::T) where {T<:AbstractNode} =
            lt(o, getval(a), getval(b))
    end
end

"""
    PriorityQueue{K,V}([o=FastMin,] T=Node{K,V})

yields a priority queue with ordering specified by `o::Ordering` and for nodes
of type `T<:AbstractNode{K,V}` with `K` the type of the keys and `V` the type
of the values encoding the priority.

Type parameters `K` and `V` may be omitted if the node type `T` is specified.

Parameters:

- `K` is key type;
- `V` is value type;
- `T<:AbstractNode{K,V}` is node type;
- `O<:Ordering` is ordering type;
- `I<:Index{K}` is the type of the index;

Keys can be:

- A linear index or a Cartesian index in another array.  The index of the
  priority queue is a, possibly, multi-dimensional array of integers with the
  same dimensions as the other array.  The stored values are the linear indices
  in the vector of nodes of the priority queue or 0 if there no such node.
  This kind of index must keep a track of the number of nodes.

- Anything else: a dictionary will be used to associate keys with indices in
  the vector of nodes of the priority queue.

To enqueue key `k` with value `v` in priority queue `pq`, all the following
are equivalent:

    pq[k] = v
    push!(pq, k => v)
    enqueue!(pq, k, v)
    enqueue!(pq, k => v)

Note that key `k` may already exists in `pq`; in that case, the value
associated with the key is updated and the queue reorderd as needed.  This is
faster than (FIXME: check this) first deleting the key and then enqueuing the
key with the new value.

To extract the key `k` and priority `v` of the node of highest priority from
the queue `pq`, call one of:

    k = dequeue!(pq)
    k, v = pop!(pq)

Method `dequeue!` may be called with a type argument `T` as `dequeue!(T,pq)` to
extract the node of highest priority and apply constructor `T` to convert it to
something else.  For example, `pop!(pq)` is implemented as `dequeue!(Pair,pq)`
and you may call `dequeue!(QuickHeaps.AbstractNode,pq)` to extract the node.
If you want to apply a function `f` taht is not a type constructor to the node
of highest priority, call:

    f(dequeue!(QuickHeaps.AbstractNode,pq))

To just examine the node of highest priority, call one of:

    k, v = peek(pq)
    k, v = first(pq)

Like the `dequeue!` method, the `peek` method may also be called with a type
argument.

A priority queue `pq` behaves like a dictionary:

    length(pq)                # yields number of nodes
    isempty(pq)               # yields whether priority queue is empty
    empty!(pq)                # empties priority queue
    keytype(pq)               # yields key type `K`
    valtype(pq)               # yields value type `V`
    pq[k] = v                 # set value `v` of key `k`
    push!(pq, k => v)         # idem.
    haskey(pq, k)             # yields whether key exists
    get(pq, k, def)           # query value at key, with default
    delete!(pq, k)            # delete node at key `k`
    keys(pq)                  # yields iterator over keys
    vals(pq)                  # yields iterator over values
    for (k,v) in pq; ...; end # loop over key-value pairs

Note that the 3 above iterators yield nodes in their storage order which is not
that of their priority.  The order is however the same for the 3 cases.

"""
struct PriorityQueue{K,V,T,O} <: AbstractPriorityQueue{K,V,T,O}
    order::O
    nodes::Vector{T}
    index::Dict{K,Int}
end

# Copy constructor.  The copy is independent from the original.
copy(pq::PriorityQueue{K,V,T,O}) where {K,V,T,O} =
    PriorityQueue{K,V,T,O}(ordering(pq), copy(nodes(pq)), copy(index(pq)))

"""
    FastPriorityQueue{V}([o=FastMin,] T=Node{Int,V}, dims...)

yields a priority queue with ordering specified by `o::Ordering` and for nodes
of type `T<:AbstractNode{Int,V}` with `V` the type of the values encoding the
priority. Type parameter `V` may be omitted if the node type `T` is specified.

The keys in this specialized priority queue are the linear or Cartesian indices
in an array of size `dims...`.  Internally, the keys are stored as linear
indices of type `Int` but the priority queue may be indexed with linear or
Cartesian indices.  For example, if `pq` is a priority queue of this kind built
with `dims = (3,4,5)`, then all the following expressions refer to the same
key:

    pq[44]
    pq[2,3,4]
    pq[CartesianIndex(2,3,4)]

With `n` nodes, the storage of this kind of priority queue is
`prod(dims)*sizeof(Int) + n*sizeof(T)` bytes.

The method

    haskey(pq, k)

yields whether key `k` exists in priority queue `pq`.  Note that `false` is
returned, if key `k` is an invalid key (out of bounds).  This is unlike the
syntax `pq[k]` which would throw an exception in that case.

"""
struct FastPriorityQueue{V,T<:AbstractNode{Int,V},
                         O,N} <: AbstractPriorityQueue{Int,V,T,O}
    order::O
    nodes::Vector{T}
    index::Array{Int,N}
end

# Copy constructor.  The copy is independent from the original.
copy(pq::FastPriorityQueue{V,T,O,N}) where {V,T,O,N} =
    FastPriorityQueue{V,T,O,N}(ordering(pq), copy(nodes(pq)), copy(index(pq)))

# Constructors for PriorityQueue instances.

function PriorityQueue{K,V}(o::O = DefaultOrdering,
                            ::Type{T} = Node{K,V}) where {K,V,
                                                          T<:AbstractNode{K,V},
                                                          O<:Ordering}
    return PriorityQueue{K,V,T,O}(o, T[], Dict{K,V}())
end

PriorityQueue{K,V}(::Type{T}) where {K,V,T<:AbstractNode{K,V}} =
    PriorityQueue{K,V}(FastMin, T)

PriorityQueue{K}(::Type{T}) where {K,V,T<:AbstractNode{K,V}} =
    PriorityQueue{K,V}(T)

PriorityQueue(::Type{T}) where {K,V,T<:AbstractNode{K,V}} =
    PriorityQueue{K,V}(T)

PriorityQueue{K}(o::Ordering, ::Type{T}) where {K,V,T<:AbstractNode{K,V}} =
    PriorityQueue{K,V}(o, T)

PriorityQueue(o::Ordering, ::Type{T}) where {K,V,T<:AbstractNode{K,V}} =
    PriorityQueue{K,V}(o, T)


# Constructors for FastPriorityQueue instances.

FastPriorityQueue{V}(dims::Integer...) where {V} =
    FastPriorityQueue{V}(dims)

FastPriorityQueue{V}(o::Ordering, dims::Integer...) where {V} =
    FastPriorityQueue{V}(o, dims)

FastPriorityQueue{V}(T::Type{<:AbstractNode{Int,V}}, dims::Integer...) where {V} =
    FastPriorityQueue{V}(T, dims)

FastPriorityQueue(T::Type{<:AbstractNode{Int,<:Any}}, dims::Integer...) =
    FastPriorityQueue(T, dims)

FastPriorityQueue{V}(o::Ordering, T::Type{<:AbstractNode{Int,V}}, dims::Integer...) where {V} =
    FastPriorityQueue{V}(o, T, dims)

FastPriorityQueue(o::Ordering, T::Type{<:AbstractNode{Int,<:Any}}, dims::Integer...) =
    FastPriorityQueue(o, T, dims)

FastPriorityQueue{V}(dims::Tuple{Vararg{Integer}}) where {V} =
    FastPriorityQueue(Node{Int,V}, dims)

FastPriorityQueue{V}(o::Ordering, dims::Tuple{Vararg{Integer}}) where {V} =
    FastPriorityQueue(o, Node{Int,V}, dims)

FastPriorityQueue{V}(o::Ordering, T::Type{<:AbstractNode{Int,V}}, dims::Tuple{Vararg{Integer}}) where {V} =
    FastPriorityQueue(o, T, dims)

FastPriorityQueue{V}(T::Type{<:AbstractNode{Int,V}}, dims::Tuple{Vararg{Integer}}) where {V} =
    FastPriorityQueue(T, dims)

FastPriorityQueue(T::Type{<:AbstractNode{Int,V}}, dims::Tuple{Vararg{Integer}}) where {V} =
    FastPriorityQueue(FastMin, T, dims)

FastPriorityQueue(o::O, T::Type{<:AbstractNode{Int,V}}, dims::NTuple{N,Integer}) where {O<:Ordering,V,N} =
    FastPriorityQueue{V,T,O,N}(o, T[], zeros(Int, dims))

#show(io::IO, ::MIME"text/plain", pq::AbstractPriorityQueue) =
#    print(io, "priority queue of type ", nameof(typeof(pq)),
#          " with ", length(pq), " node(s)")

show(io::IO, ::MIME"text/plain", pq::PriorityQueue{K,V}) where {K,V} =
    print(io, typename(pq), " of type ", nameof(typeof(pq)), "{", nameof(K),
          ",", nameof(V), "} with ", length(pq), " node(s)")

show(io::IO, ::MIME"text/plain", pq::FastPriorityQueue{V}) where {V} =
    print(io, typename(pq), " of type ", nameof(typeof(pq)), "{", nameof(V),
          "} with ", length(pq), " node(s)")

ordering(pq::AbstractPriorityQueue)  = getfield(pq, :order)
nodes(pq::AbstractPriorityQueue) = getfield(pq, :nodes)
index(pq::AbstractPriorityQueue) = getfield(pq, :index)

length(pq::AbstractPriorityQueue) = length(nodes(pq))

isempty(pq::AbstractPriorityQueue) = (length(pq) ≤ 0)

keytype(pq::AbstractPriorityQueue) = keytype(typeof(pq))
keytype(::Type{<:AbstractPriorityQueue{K,V}}) where {K,V} = K

valtype(pq::AbstractPriorityQueue) = valtype(typeof(pq))
valtype(::Type{<:AbstractPriorityQueue{K,V}}) where {K,V} = V

haskey(pq::AbstractPriorityQueue, key) = (heap_index(pq, key) != 0)

function get(pq::AbstractPriorityQueue, key, def)
    n = length(pq)
    if n > 0
        i = heap_index(pq, key)
        if in_range(i, n) # FIXME: Testing that i > 0 should be sufficient.
            @inbounds x = getindex(nodes(pq), i)
            return Tuple(x)
        end
    end
    return def
end

function delete!(pq::AbstractPriorityQueue, key)
    n = length(pq)
    if n > 0
        i = heap_index(pq, key)
        if in_range(i, n) # FIXME: Testing that i > 0 should be sufficient.
            A = nodes(pq)
            @inbounds y = A[i] # node to be deleted
            if i < n
                # Replace the deleted node by the last node in the heap and
                # up-/down-heapify to restore the binary heap structure.
                @inbounds x = A[n] # last node
                o = ordering(pq)
                if lt(o, y, x)
                    # Heap structure _above_ deleted node is already valid.
                    unsafe_heapify_down!(pq, i, x, n - 1)
                else
                    # Heap structure _below_ deleted node is already valid.
                    unsafe_heapify_up!(pq, i, x)
                end
            end
            unsafe_shrink!(pq, n - 1)
            unsafe_delete_key!(pq, getkey(y))
        end
    end
    return pq
end

first(pq::AbstractPriorityQueue) = peek(pq)
peek(pq::AbstractPriorityQueue) = peek(Pair, pq)

# FIXME: Same code as for binary heaps.
function peek(T::Type, pq::AbstractPriorityQueue)
    isempty(pq) && throw_argument_error(typename(pq), " is empty")
    @inbounds x = getindex(nodes(pq), 1)
    return T(x)
end

function empty!(pq::PriorityQueue)
    empty!(nodes(pq))
    empty!(index(pq))
    return pq
end

function empty!(pq::FastPriorityQueue)
    empty!(nodes(pq))
    fill!(index(pq), 0)
    return pq
end

# Private structure used by iterators on priority queues.
struct _PriorityQueueIterator{F,T<:AbstractPriorityQueue}
    f::F
    pq::T
end

IteratorEltype(itr::_PriorityQueueIterator) = IteratorEltype(typeof(itr))
IteratorEltype(::Type{<:AbstractPriorityQueue}) = HasEltype()
eltype(itr::_PriorityQueueIterator) = eltype(typeof(itr))
function eltype(::Type{<:_PriorityQueueIterator{F,T}}
                ) where {K,V,F<:typeof(getkey),T<:AbstractPriorityQueue{K,V}}
    return K
end
function eltype(::Type{<:_PriorityQueueIterator{F,T}}
                ) where {K,V,F<:typeof(getval),T<:AbstractPriorityQueue{K,V}}
    return V
end
eltype(::Type{<:_PriorityQueueIterator}) = Any

IteratorSize(itr::_PriorityQueueIterator) = IteratorSize(typeof(itr))
IteratorSize(::Type{<:_PriorityQueueIterator}) = HasLength()
length(itr::_PriorityQueueIterator) = length(itr.pq)

# Unordered iterators.  NOTE: Both `keys` and `values` shall however return the
# elements in the same order.
function iterate(pq::AbstractPriorityQueue, i::Int = 1)
    i ≤ length(pq) || return nothing
    @inbounds x = getindex(nodes(pq), i)
    return Pair(x), i + 1
end
keys(pq::AbstractPriorityQueue) = _PriorityQueueIterator(getkey, pq)
values(pq::AbstractPriorityQueue) = _PriorityQueueIterator(getval, pq)
function Base.iterate(itr::_PriorityQueueIterator, i::Int = 1)
    i ≤ length(itr.pq) || return nothing
    @inbounds x = getindex(nodes(itr.pq), i)
    return itr.f(x), i + 1
end

pop!(pq::AbstractPriorityQueue) = dequeue!(Pair, pq)

dequeue_pair!(pq::AbstractPriorityQueue) = dequeue!(Pair, pq)

dequeue!(pq::AbstractPriorityQueue) = getkey(dequeue!(AbstractNode, pq))

# This is almost the same code as pop! for a binary heap.
function dequeue!(T::Type, pq::AbstractPriorityQueue)
    n = length(pq)
    n ≥ 1 || throw_argument_error(typename(pq), " is empty")
    A = nodes(pq)
    @inbounds x = A[1]
    if n > 1
        # Peek the last node and down-heapify starting at the root of the
        # binary heap to insert it.
        @inbounds y = A[n]
        unsafe_heapify_down!(pq, 1, y, n - 1)
    end
    unsafe_delete_key!(pq, getkey(x))
    unsafe_shrink!(pq, n - 1)
    return T(x)
end

# FIXME: Same code as for a binary heap.
push!(pq::AbstractPriorityQueue, ::Tuple{}) = pq

# FIXME: Same code as for a binary heap.
function push!(pq::AbstractPriorityQueue, args...)
    for arg in args
        push!(pq, arg)
    end
    return pq
end

push!(pq::AbstractPriorityQueue, pair::Pair) = enqueue!(pq, pair)

push!(pq::AbstractPriorityQueue{<:Any,<:Any,T}, node::T) where {T} =
    enqueue!(pq, node)

getindex(pq::AbstractPriorityQueue, ::Tuple{}) = throw_missing_key()

setindex!(pq::AbstractPriorityQueue, val, ::Tuple{}) = throw_missing_key()

throw_missing_key() = throw_argument_error("missing key")

function getindex(pq::PriorityQueue, key)
    i = heap_index(pq, key)
    # FIXME: Testing that i > 0 should be sufficient.
    in_range(i, length(pq)) || throw_argument_error(
        typename(pq), " has no node with key ", key)
    @inbounds r = getindex(nodes(pq), i)
    return getval(r)
end

setindex!(pq::PriorityQueue, val, key) = enqueue!(pq, key, val)

# Union of types that can be used to index fast priority queues.
const FastIndex = Union{Integer,CartesianIndex}

# For indexing fast priority queues, we first convert the key into a linear
# index (using the current bounds checking state).
for keytype in (:Integer, :(FastIndex...))
    @eval begin
        @inline @propagate_inbounds function getindex(pq::FastPriorityQueue,
                                                      key::$keytype)
            k = linear_index(pq, key)
            @inbounds i = getindex(index(pq), k)
            A = nodes(pq)
            if in_range(i, A)
                @inbounds x = A[i]
                return getval(x)
            end
            throw_argument_error(typename(pq), " has no node with key ",
                                 normalize_key(pq, key))
        end
        @inline @propagate_inbounds function setindex!(pq::FastPriorityQueue,
                                                       val,
                                                       key::$keytype)
            return enqueue!(pq, key, val)
        end
    end
end

normalize_key(pq::FastPriorityQueue, key::Integer) = to_int(key)
normalize_key(pq::FastPriorityQueue, key::Tuple{Vararg{FastIndex}}) =
    to_indices(index(pq), key)

"""
    to_key(pq, k)

converts the key `k` to the type suitable for priority queue `pq`.

"""
to_key(pq::AbstractPriorityQueue{K,V}, key::K) where {K,V} = key
to_key(pq::AbstractPriorityQueue{K,V}, key) where {K,V} = to_type(K, key)

"""
    to_val(pq, v)

converts the value `v` to the type suitable for priority queue `pq`.

"""
to_val(pq::AbstractPriorityQueue{K,V}, val::V) where {K,V} = val
to_val(pq::AbstractPriorityQueue{K,V}, val) where {K,V} = to_type(V, val)

"""
    to_node(pq, k, v)

converts the the key `k` and the value `v` into a node type suitable for
priority queue `pq`.

"""
to_node(pq::AbstractPriorityQueue{K,V,T}, key, val) where {K,V,T} =
    T(to_key(pq, key), to_val(pq, val))

"""
    heap_index(pq, k) -> i

yields the index in the binary heap backing the storage of the nodes of the
priority queue `pq` of the key `k`.  If the key is not in priority queue, `i =
0` is returned, otherwise `i ∈ 1:n` with `n = length(pq)` is returned.

The `heap_index` method is used to implement `haskey`, `get`, and `delete!`
methods for priority queues.  The `heap_index` method shall be specialized for
any concrete sub-types of `QuickHeaps.AbstractPriorityQueue`.

""" heap_index # NOTE: `heap_index` ~ `ht_keyindex` in `base/dict.jl`

# By default, pretend that the key is missing.
heap_index(pq::AbstractPriorityQueue, key) = 0

heap_index(pq::PriorityQueue, key) = get(index(pq), key, 0)

function heap_index(pq::FastPriorityQueue, key::Integer)
    k = to_int(key)
    I = index(pq)
    in_range(k, I) || return 0
    @inbounds i = I[k]
    return i
end

function heap_index(pq::FastPriorityQueue,
                    key::CartesianIndex)
    I = index(pq)
    if checkbounds(Bool, I, key)
        @inbounds i = I[key]
        return i
    end
    return 0
end

function heap_index(pq::FastPriorityQueue,
                    key::Tuple{Vararg{FastIndex}})
    I = index(pq)
    if checkbounds(Bool, I, key...)
        @inbounds i = I[key...]
        return i
    end
    return 0
end

"""
    linear_index(pq, k)

converts key `k` into a linear index suitable for the fast priority queue `pq`.
The key can be a linear index or a multi-dimensional index (anything accepted
by `to_indices`).  The current settings for bounds checking are used.

"""
@inline @propagate_inbounds linear_index(pq::FastPriorityQueue, key::Integer) =
    # Convert to Int, then re-call linear_index for bound checking.  Note that
    # the type assertion performed by `to_type` avoids infinite recursion.
    linear_index(pq, to_type(Int, key))

@inline function linear_index(pq::FastPriorityQueue, key::Int)
    @boundscheck checkbounds(index(pq), key)
    return key
end

@inline @propagate_inbounds function linear_index(pq::FastPriorityQueue,
                                                  key::Tuple{Vararg{FastIndex}})
    # FIXME: Shall we store the linear_indices (a small object) in the priority
    #        queue directly?
    return LinearIndices(index(pq))[key...] # also does the bound checking
end

linear_index(pq::FastPriorityQueue, key) = throw_invalid_key(pq, key)

@noinline throw_invalid_key(pq::AbstractPriorityQueue, key) = throw_argument_error(
    "invalid key of type ", typeof(key), " for ", nameof(typeof(pq)))

@noinline throw_invalid_key(pq::FastPriorityQueue, key) = throw_argument_error(
    "invalid key of type ", typeof(key), " for ", nameof(typeof(pq)),
    " expecting a linear index, an ", ndims(index(pq)),
    "-dimensional Cartesian index")

# The following is to allow the syntax enqueue!(pq, key=>val)
enqueue!(pq::AbstractPriorityQueue, pair::Pair) =
    enqueue!(pq, pair.first, pair.second)

# For a general purpose priority queue, build the node then enqueue.
enqueue!(pq::PriorityQueue, key, val) = enqueue!(pq, to_node(pq, key, val))
enqueue!(pq::PriorityQueue{K,V,T}, x::T) where {K,V,T} =
    unsafe_enqueue!(pq, x, get(index(pq), getkey(x), 0))

# For a fast priority queue, converts the key into a linear index, then enqueue.
@inline @propagate_inbounds function enqueue!(pq::FastPriorityQueue, key, val)
    k = linear_index(pq, key) # not to_key
    v = to_val(pq, val)
    x = to_node(pq, k, v)
    @inbounds i = getindex(index(pq), k)
    return unsafe_enqueue!(pq, x, i)
end

enqueue!(pq::FastPriorityQueue{V,T}, x::T) where {V,T} =
    enqueue!(pq, getkey(x), getval(x))

"""
    unsafe_enqueue!(pq, x, i) -> pq

stores node `x` in priority queue `pq` at index `i` and returns the priority
queue.  The argument `i` is an index in the binary heap backing the storage of
the nodes of the priority queue.  Index `i` is determined by the key `k` of the
node `x` and by the current state of the priority queue.  If `i` is not a valid
index in the binary heap, a new node is added; otherwise, the node at index `i`
in the binary heap is replaced by `x`.  In any cases, the binary heap is
reordered as needed.

This function is *unsafe* because it assumes that the key `k` of the node `x`
is valid (e.g. it is not out of bounds for fast priority queues) in the sense
that `I[k]` is valid for the index `I` of the priority queue.

"""
function unsafe_enqueue!(pq::AbstractPriorityQueue{K,V,T},
                         x::T, i::Int) where {K,V,T}
    A = nodes(pq)
    if in_range(i, A)
        # The key alreay exists, replace its node.
        @inbounds y = A[i] # node to be replaced
        o = ordering(pq)
        if lt(o, y, x)
            # Heap structure _above_ replaced node will remain valid,
            # down-heapify to fix the heap structure at and _below_ the node.
            unsafe_heapify_down!(pq, i, x)
        else
            # Heap structure _below_ replaced node will remain valid,
            # up-heapify to fix the heap structure at and _above_ the node.
            unsafe_heapify_up!(pq, i, x)
        end
    else
        # No such key already exists.  Create a new slot at the end of the node
        # list and up-heapify to fix the structure and insert the new node.
        n = length(pq) + 1
        unsafe_heapify_up!(unsafe_grow!(pq, n), n, x)
    end
    return pq
end

"""
    unsafe_enqueue!(dir, pq, k, v) -> pq

requeues key `k` at priority `v` in priority queue `pq` forcing the
heapification of the binary heap backing the storage of the nodes of `pq` in
the direction `dir`.  A node with the same key `k` must already exists in the
queue.  If `dir = Val(:down)`, it is assumed that the new priority `v` of the
key `k` is less than the former priority; if `dir = Val(:up)`, it is assumed
that the new priority is greater than the former one.

This specialization of the `unsafe_enqueue!` method is *unsafe* because the
binary heap backing the storage of the nodes may be left with an invalid
structure if `dir` is wrong.

"""
@inline @propagate_inbounds function unsafe_enqueue!(dir::Union{Val{:down},
                                                                Val{:up}},
                                                     pq::AbstractPriorityQueue,
                                                     k, v)
    i = heap_index(pq, k)
    in_range(i, length(pq)) || throw_argument_error(
        "key ", key, " does not exists in ", typename(pq))
    @inbounds x = getindex(nodes(pq), i)
    unsafe_heapify!(dir, pq, x, i)
    return pq
end

function unsafe_heapify!(::Val{:down},
                         pq::AbstractPriorityQueue{K,V,T},
                         x::T, i::Int) where {K,V,T}
    unsafe_heapify_down!(pq, i, x)
end

function unsafe_heapify!(::Val{:up},
                         pq::AbstractPriorityQueue{K,V,T},
                         x::T, i::Int, ::Val{:up}) where {K,V,T}
    unsafe_heapify_up!(pq, i, x)
end

"""
    unsafe_grow!(pq, n) -> pq

grows the size of the binary heap backing the storage of the nodes of the
priority queue `pq` to be `n` and returns the priority queue object.

"""
unsafe_grow!(pq::Union{PriorityQueue,FastPriorityQueue}, n::Int) = begin
    resize!(nodes(pq), n)
    return pq
end

"""
    unsafe_shrink!(pq, n)

shrinks the size of the binary heap backing the storage of the nodes of the
priority queue `pq` to be `n`.

"""
unsafe_shrink!(pq::Union{PriorityQueue,FastPriorityQueue}, n::Int) =
    resize!(nodes(pq), n)

"""
    unsafe_delete_key!(pq, k)

deletes key `k` from the index of the priority queue `pq` assuming `k`
is valid.

"""
unsafe_delete_key!(pq::AbstractPriorityQueue{K}, key::K) where {K} =
    unsafe_delete_key!(index(pq), key)

# Specialized version for the type of the index.
unsafe_delete_key!(I::Array{Int}, key::Int) = @inbounds I[key] = 0
unsafe_delete_key!(I::AbstractDict, key) = delete!(I, key)

@inline function unsafe_heapify_down!(pq::AbstractPriorityQueue{K,V,T},
                                      i::Int, x::T,
                                      n::Int = length(pq)) where {K,V,T}
    o = ordering(pq)
    A = nodes(pq)
    I = index(pq)
    # FIXME: @inbounds
    begin
        while (l = heap_left(i)) ≤ n
            j = (r = heap_right(i)) > n || lt(o, A[l], A[r]) ? l : r
            lt(o, A[j], x) || break
            I[getkey(A[j])] = i
            A[i] = A[j]
            i = j
        end
        I[getkey(x)] = i
        A[i] = x
    end
end

@inline function unsafe_heapify_up!(pq::AbstractPriorityQueue{K,V,T},
                                    i::Int, x::T) where {K,V,T}
    o = ordering(pq)
    A = nodes(pq)
    I = index(pq)
    # FIXME: @inbounds
    begin
        while (j = heap_parent(i)) ≥ 1 && lt(o, x, A[j])
            I[getkey(A[j])] = i
            A[i] = A[j]
            i = j
        end
        I[getkey(x)] = i
        A[i] = x
    end
end
