"""
    QuickHeaps.AbstractPriorityQueue{K,V,O}

is the super type of priority queues with nodes consisting in pairs of keys of type `K`,
priority values of type `V`, and ordering of type `O<:Base.Ordering`.

Priority queues implement an API similar to dictionaries with the additional feature of
maintaining an ordered structure so that getting the node of highest priority costs `O(1)`
while pushing a node costs `O(log(n))` with `n` the size of the queue. See online
documentation for more details.

Package `QuickHeaps` provides two concrete types of priority queues: [`PriorityQueue`](@ref)
for any kind of keys and [`FastPriorityQueue`](@ref) for keys which are analogous to array
indices.

"""
abstract type AbstractPriorityQueue{K,V,O<:Ordering} <: AbstractDict{K,V} end

default_ordering(::Type{<:AbstractPriorityQueue}) = Forward

typename(::Type{<:AbstractPriorityQueue}) = "priority queue"

"""
    PriorityQueue{K,V}([o=Forward,] T=Node{K,V})

yields a priority queue for keys of type `K` and priority values of type `V`. Optional
arguments `o::Ordering` and `T<:AbstractNode{K,V}` are to specify the ordering of values and
type of nodes to store key-value pairs. Type parameters `K` and `V` may be omitted if the
node type `T` is specified.

Having a specific node type may be useful to specialize the `QuickHeaps.lt` method which is
called to determine the order.

If keys are analogous to array indices (linear or Cartesian), [`FastPriorityQueue`](@ref)
may provide a faster alternative.

"""
struct PriorityQueue{K,V,O,T} <: AbstractPriorityQueue{K,V,O}
    order::O
    nodes::Vector{T}
    index::Dict{K,Int}
end

# Copy constructor. The copy is independent from the original.
copy(pq::PriorityQueue{K,V,O,T}) where {K,V,O,T} =
    PriorityQueue{K,V,O,T}(ordering(pq), copy(nodes(pq)), copy(index(pq)))

"""
    FastPriorityQueue{V}([o=Forward,] [T=Node{Int,V},] dims...)

yields a priority queue for keys analogous of indices in an array of size `dims...` and
priority values of type `V`. Optional arguments `o::Ordering` and `T<:AbstractNode{Int,V}`
are to specify the ordering of values and type of nodes to store key-value pairs (the key is
stored as a linear index of type `Int`). Type parameter `V` may be omitted if the node type
`T` is specified.

See [`PriorityQueue`](@ref) if keys cannot be assumed to be array indices.

"""
struct FastPriorityQueue{V,N,O,
                         T<:AbstractNode{Int,V}} <: AbstractPriorityQueue{Int,V,O}
    order::O
    nodes::Vector{T}
    index::Array{Int,N}
end

# Copy constructor.  The copy is independent from the original.
copy(pq::FastPriorityQueue{V,N,O,T}) where {V,N,O,T} =
    FastPriorityQueue{V,N,O,T}(ordering(pq), copy(nodes(pq)), copy(index(pq)))

# Constructors for PriorityQueue instances.

function PriorityQueue{K,V}(o::O = default_ordering(PriorityQueue),
                            ::Type{T} = Node{K,V}) where {K,V,O<:Ordering,
                                                          T<:AbstractNode{K,V}}
    return PriorityQueue{K,V,O,T}(o, T[], Dict{K,V}())
end

PriorityQueue{K,V}(::Type{T}) where {K,V,T<:AbstractNode{K,V}} =
    PriorityQueue{K,V}(default_ordering(PriorityQueue), T)

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
    FastPriorityQueue(default_ordering(FastPriorityQueue), T, dims)

FastPriorityQueue(o::O, T::Type{<:AbstractNode{Int,V}}, dims::NTuple{N,Integer}) where {O<:Ordering,V,N} =
    FastPriorityQueue{V,N,O,T}(o, T[], zeros(Int, dims))

#show(io::IO, ::MIME"text/plain", pq::AbstractPriorityQueue) =
#    print(io, "priority queue of type ", nameof(typeof(pq)),
#          " with ", length(pq), " node(s)")

show(io::IO, ::MIME"text/plain", pq::PriorityQueue{K,V}) where {K,V} =
    print(io, typename(pq), " of type ", nameof(typeof(pq)), "{", nameof(K),
          ",", nameof(V), "} with ", length(pq), " node(s)")

show(io::IO, ::MIME"text/plain", pq::FastPriorityQueue{V}) where {V} =
    print(io, typename(pq), " of type ", nameof(typeof(pq)), "{", nameof(V),
          "} with ", length(pq), " node(s)")

"""
    QuickHeaps.ordering(pq) -> o

yields the object `o` specifying the ordering of priority values in the priority queue `pq`.

"""
ordering(pq::AbstractPriorityQueue)  = getfield(pq, :order)

"""
    QuickHeaps.index(pq) -> I

yields the object `I` storing the key-index association in priority queue `pq`. This object
can be used as `I[key]` to yield , for a given key, the index in `QuickHeaps.nodes(pq)`, the
associated binary heap.

"""
index(pq::AbstractPriorityQueue) = getfield(pq, :index)

"""
    QuickHeaps.nodes(pq)

yields the array backing the storage of the nodes of priority queue `pq`.

"""
nodes(pq::AbstractPriorityQueue) = getfield(pq, :nodes)

node_type(pq::AbstractPriorityQueue) = node_type(typeof(pq))
node_type(::Type{<:PriorityQueue{K,V,O,T}}) where {K,V,O,T} = T
node_type(::Type{<:FastPriorityQueue{V,N,O,T}}) where {V,N,O,T} = T

length(pq::AbstractPriorityQueue) = length(nodes(pq))

isempty(pq::AbstractPriorityQueue) = (length(pq) ≤ 0)

keytype(pq::AbstractPriorityQueue) = keytype(typeof(pq))
keytype(::Type{<:AbstractPriorityQueue{K,V}}) where {K,V} = K

valtype(pq::AbstractPriorityQueue) = valtype(typeof(pq))
valtype(::Type{<:AbstractPriorityQueue{K,V}}) where {K,V} = V

haskey(pq::AbstractPriorityQueue, key) = (heap_index(pq, key) != 0)

for (func, getter) in ((:get, :get_val), (:getkey, :get_key))
    @eval function $func(pq::AbstractPriorityQueue, key, def)
        n = length(pq)
        if n > 0
            i = heap_index(pq, key)
            if in_range(i, n) # FIXME: Testing that i > 0 should be sufficient.
                @inbounds x = getindex(nodes(pq), i)
                return $getter(x)
            end
        end
        return def
    end
end

function delete!(pq::AbstractPriorityQueue, key)
    n = length(pq)
    if n > 0
        i = heap_index(pq, key)
        if in_range(i, n) # FIXME: Testing that i > 0 should be sufficient.
            A = nodes(pq)
            @inbounds k = get_key(A[i]) # key to be deleted
            if i < n
                # Replace the deleted node by the last node in the heap and up-/down-heapify
                # to restore the binary heap structure. We cannot assume that the deleted
                # node data be accessible nor valid, so we explicitely replace it before
                # deciding in which direction to go and reheapify. Also see
                # `unsafe_enqueue!`.
                @inbounds x = A[n] # get last node
                @inbounds A[i] = x # replace deleted node
                o = ordering(pq)
                if i ≤ 1 || lt(o, (@inbounds A[heap_parent(i)]), x)
                    unsafe_heapify_down!(pq, i, x, n - 1)
                else
                    unsafe_heapify_up!(pq, i, x)
                end
            end
            unsafe_shrink!(pq, n - 1)
            unsafe_delete_key!(pq, k)
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
struct PriorityQueueIterator{F,Q<:AbstractPriorityQueue}
    f::F
    pq::Q
end

IteratorEltype(itr::PriorityQueueIterator) = IteratorEltype(typeof(itr))
IteratorEltype(::Type{<:PriorityQueueIterator}) = HasEltype()
eltype(itr::PriorityQueueIterator) = eltype(typeof(itr))
eltype(::Type{<:PriorityQueueIterator{typeof(get_key),Q}}) where {Q} = keytype(Q)
eltype(::Type{<:PriorityQueueIterator{typeof(get_val),Q}}) where {Q} = valtype(Q)
eltype(::Type{<:PriorityQueueIterator{F,Q}}) where {F,Q} = Any

IteratorSize(itr::PriorityQueueIterator) = IteratorSize(typeof(itr))
IteratorSize(::Type{<:PriorityQueueIterator}) = HasLength()
length(itr::PriorityQueueIterator) = length(itr.pq)

# Unordered iterators. NOTE: All iterators shall however return the elements in the same
# order.
function iterate(pq::AbstractPriorityQueue, i::Int = 1)
    in_range(i, length(pq)) || return nothing
    @inbounds x = getindex(nodes(pq), i)
    return Pair(x), i + 1
end
keys(pq::AbstractPriorityQueue) = PriorityQueueIterator(get_key, pq)
values(pq::AbstractPriorityQueue) = PriorityQueueIterator(get_val, pq)
function Base.iterate(itr::PriorityQueueIterator, i::Int = 1)
    in_range(i, length(itr.pq)) || return nothing
    @inbounds x = getindex(nodes(itr.pq), i)
    return itr.f(x), i + 1
end

pop!(pq::AbstractPriorityQueue) = dequeue_pair!(pq)

"""
    dequeue!(pq) -> key

removes the root node from the priority queue `pq` and returns its key.

Also see [`dequeue_node!`](@ref) and [`dequeue_pair!`](@ref).

"""
dequeue!(pq::AbstractPriorityQueue) = get_key(dequeue_node!(pq))

"""
    dequeue_node!(pq) -> node

removes and returns the root node from the priority queue `pq`.

Also see [`dequeue!`](@ref) and [`dequeue_pair!`](@ref).

"""
function dequeue_node!(pq::AbstractPriorityQueue)
    # The code is almost the same as pop! for a binary heap.
    n = length(pq)
    n ≥ 1 || throw_argument_error(typename(pq), " is empty")
    A = nodes(pq)
    @inbounds x = A[1]
    if n > 1
        # Peek the last node and down-heapify starting at the root of the binary heap to
        # insert it.
        @inbounds y = A[n]
        unsafe_heapify_down!(pq, 1, y, n - 1)
    end
    unsafe_delete_key!(pq, get_key(x))
    unsafe_shrink!(pq, n - 1)
    return x
end

"""
    dequeue_pair!(pq) -> (key => val)

removes the root node from the priority queue `pq` and returns it as a key-value `Pair`.
This is the same as `pop!(pq)`.

Also see [`dequeue!`](@ref) and [`dequeue_node!`](@ref).

"""
dequeue_pair!(pq::AbstractPriorityQueue) = Pair(dequeue_node!(pq))

# For AbstractDict, pushing pair(s) is already implemented via setindex! Implement push! for
# 2-tuples and nodes in a similar way as for AbstractDict.
push!(pq::AbstractPriorityQueue, x::AbstractNode) =
    enqueue!(pq, get_key(x), get_val(x))
push!(pq::AbstractPriorityQueue, x::Tuple{Any,Any}) =
    enqueue!(pq, x[1], x[2])
for T in (AbstractNode, Tuple{Any,Any})
    @eval begin
        push!(pq::AbstractPriorityQueue, a::$T, b::$T) =
            push!(push!(pq, a), b)
        push!(pq::AbstractPriorityQueue, a::$T, b::$T, c::$T...) =
            push!(push!(push!(pq, a), b), c...)
    end
end

function getindex(pq::PriorityQueue, key)
    i = heap_index(pq, key)
    # FIXME: Testing that i > 0 should be sufficient.
    in_range(i, length(pq)) || throw_argument_error(
        typename(pq), " has no node with key ", key)
    @inbounds r = getindex(nodes(pq), i)
    return get_val(r)
end

setindex!(pq::PriorityQueue, val, key) = enqueue!(pq, key, val)

# Union of types that can be used to index fast priority queues.
const FastIndex = Union{Integer,CartesianIndex}

# For indexing fast priority queues, we first convert the key into a linear index (using the
# current bounds checking state).
for keytype in (:Integer, :(FastIndex...))
    @eval begin
        @inline @propagate_inbounds function getindex(pq::FastPriorityQueue,
                                                      key::$keytype)
            k = linear_index(pq, key)
            @inbounds i = getindex(index(pq), k)
            A = nodes(pq)
            if in_range(i, A)
                @inbounds x = A[i]
                return get_val(x)
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

normalize_key(pq::FastPriorityQueue, key::Integer) = as(Int, key)
normalize_key(pq::FastPriorityQueue, key::Tuple{Vararg{FastIndex}}) =
    to_indices(index(pq), key)

"""
    Quickheaps.to_key(pq, k)

converts the key `k` to the type suitable for priority queue `pq`.

"""
to_key(pq::AbstractPriorityQueue{K,V}, key::K) where {K,V} = key
to_key(pq::AbstractPriorityQueue{K,V}, key) where {K,V} = as(K, key)

"""
    Quickheaps.to_val(pq, v)

converts the value `v` to the type suitable for priority queue `pq`.

"""
to_val(pq::AbstractPriorityQueue{K,V}, val::V) where {K,V} = val
to_val(pq::AbstractPriorityQueue{K,V}, val) where {K,V} = as(V, val)

"""
    Quickheaps.to_node(pq, k, v)

converts the the key `k` and the value `v` into a node type suitable for
priority queue `pq`.

"""
to_node(pq::AbstractPriorityQueue, (key, val)::Pair) = to_node(pq, key, val)
to_node(pq::AbstractPriorityQueue, key, val) =
    node_type(pq)(to_key(pq, key), to_val(pq, val))

"""
    Quickheaps.heap_index(pq, k) -> i

yields the index of the key `k` in the binary heap backing the storage of the nodes of the
priority queue `pq`. If the key is not in priority queue, `i = 0` is returned, otherwise `i
∈ 1:n` with `n = length(pq)` is returned.

The `heap_index` method is used to implement `haskey`, `get`, and `delete!` methods for
priority queues. The `heap_index` method shall be specialized for any concrete sub-types of
`QuickHeaps.AbstractPriorityQueue`.

""" heap_index # NOTE: `heap_index` ~ `ht_keyindex` in `base/dict.jl`

# By default, pretend that the key is missing.
heap_index(pq::AbstractPriorityQueue, key) = 0

heap_index(pq::PriorityQueue, key) = get(index(pq), key, 0)

function heap_index(pq::FastPriorityQueue, key::Integer)
    k = as(Int, key)
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
    QuickHeaps.linear_index(pq, k)

converts key `k` into a linear index suitable for the fast priority queue `pq`. The key can
be a linear index or a multi-dimensional index (anything accepted by `to_indices`). The
current settings for bounds checking are used.

"""
@inline @propagate_inbounds linear_index(pq::FastPriorityQueue, key::Integer) =
    # Convert to Int, then re-call linear_index for bound checking. Note that the type
    # assertion performed by `as` avoids infinite recursion.
    linear_index(pq, as(Int, key))

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

enqueue!(pq::AbstractPriorityQueue, key, val) = enqueue!(pq, key=>val)

# For a general purpose priority queue, build the node then enqueue.
enqueue!(pq::PriorityQueue, (key, val)::Pair) = enqueue!(pq, to_node(pq, key, val))
enqueue!(pq::PriorityQueue{K,V,O,T}, x::T) where {K,V,O,T} =
    unsafe_enqueue!(pq, x, get(index(pq), get_key(x), 0))
enqueue!(pq::PriorityQueue{K,V,O,T}, x::Any) where {K,V,O,T} =
    unsafe_enqueue!(pq, to_node(pq, get_key(x),  get_val(x))::T)

# For a fast priority queue, converts the key into a linear index, then enqueue.
@inline @propagate_inbounds function enqueue!(pq::FastPriorityQueue, (key, val)::Pair)
    k = linear_index(pq, key) # not to_key
    v = to_val(pq, val)
    x = to_node(pq, k, v)
    @inbounds i = getindex(index(pq), k)
    return unsafe_enqueue!(pq, x, i)
end

enqueue!(pq::FastPriorityQueue{V,N,O,T}, x::T) where {V,N,O,T} =
    enqueue!(pq, get_key(x), get_val(x))

"""
    QuickHeaps.unsafe_enqueue!(pq, x, i) -> pq

stores node `x` in priority queue `pq` at index `i` and returns the priority queue. The
argument `i` is an index in the binary heap backing the storage of the nodes of the priority
queue. Index `i` is determined by the key `k` of the node `x` and by the current state of
the priority queue. If `i` is not a valid index in the binary heap, a new node is added;
otherwise, the node at index `i` in the binary heap is replaced by `x`. In any cases, the
binary heap is reordered as needed.

This function is *unsafe* because it assumes that the key `k` of the node `x` is valid (e.g.
it is not out of bounds for fast priority queues) in the sense that `I[k]` is valid for the
index `I` of the priority queue.

"""
function unsafe_enqueue!(pq::AbstractPriorityQueue, x, i::Int)
    A = nodes(pq)
    if in_range(i, A)
        # The key already exists. Replace the node in the heap by the new node and
        # up-/down-heapify to restore the binary heap structure. We cannot assume that the
        # replaced node data be accessible nor valid, so we explicitly replace it before
        # deciding in which direction to go and re-heapify. Also see `delete!`.
        @inbounds A[i] = x # replace deleted node
        o = ordering(pq)
        if i ≤ 1 || lt(o, (@inbounds A[heap_parent(i)]), x)
            unsafe_heapify_down!(pq, i, x)
        else
            unsafe_heapify_up!(pq, i, x)
        end
    else
        # No such key already exists. Create a new slot at the end of the node list and
        # up-heapify to fix the structure and insert the new node.
        n = length(pq) + 1
        unsafe_heapify_up!(unsafe_grow!(pq, n), n, x)
    end
    return pq
end

"""
    QuickHeaps.unsafe_grow!(pq, n) -> pq

grows the size of the binary heap backing the storage of the nodes of the priority queue
`pq` to be `n` and returns the priority queue object.

"""
unsafe_grow!(pq::Union{PriorityQueue,FastPriorityQueue}, n::Int) = begin
    resize!(nodes(pq), n)
    return pq
end

"""
    QuickHeaps.unsafe_shrink!(pq, n)

shrinks the size of the binary heap backing the storage of the nodes of the priority queue
`pq` to be `n`.

"""
unsafe_shrink!(pq::Union{PriorityQueue,FastPriorityQueue}, n::Int) =
    resize!(nodes(pq), n)

"""
    QuickHeaps.unsafe_delete_key!(pq, k)

deletes key `k` from the index of the priority queue `pq` assuming `k` is valid.

"""
unsafe_delete_key!(pq::AbstractPriorityQueue{K}, key::K) where {K} =
    unsafe_delete_key!(index(pq), key)

# Specialized version for the type of the index.
unsafe_delete_key!(I::Array{Int}, key::Int) = @inbounds I[key] = 0
unsafe_delete_key!(I::AbstractDict, key) = delete!(I, key)

@inline function unsafe_heapify_down!(pq::AbstractPriorityQueue, i::Int,
                                      x, n::Int = length(pq))
    o = ordering(pq)
    A = nodes(pq)
    I = index(pq)
    @inbounds begin
        while (l = heap_left(i)) ≤ n
            j = (r = heap_right(i)) > n || lt(o, A[l], A[r]) ? l : r
            lt(o, A[j], x) || break
            I[get_key(A[j])] = i
            A[i] = A[j]
            i = j
        end
        I[get_key(x)] = i
        A[i] = x
    end
end

@inline function unsafe_heapify_up!(pq::AbstractPriorityQueue, i::Int, x)
    o = ordering(pq)
    A = nodes(pq)
    I = index(pq)
    @inbounds begin
        while (j = heap_parent(i)) ≥ 1 && lt(o, x, A[j])
            I[get_key(A[j])] = i
            A[i] = A[j]
            i = j
        end
        I[get_key(x)] = i
        A[i] = x
    end
end
