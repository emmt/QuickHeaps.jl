"""
    QuickHeaps.AbstractNode{K,V}

is the super-type of nodes with a key of type `K` and a value of type `V`.
Nodes can be used in binary heaps and priority queues where they are used to
store key-value pairs and, by specializing the `Base.lt` method, to implement
how the nodes are ordered.

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
getkey(x::Node) = getfield(x, :key)
getval(x::Node) = getfield(x, :val)

Pair(x::AbstractNode) = getkey(x) => getval(x)
Tuple(x::AbstractNode) = (getkey(x), getval(x))

# Nodes are sorted according to their values.
for O in (:Ordering, :FastForwardOrdering)
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

    k, v = dequeue!(pq)
    k, v = pop!(pq)

To just examine the node node of highest priority, call one of:

    k, v = peek(pq)
    k, v = first(pq)

"""
struct PriorityQueue{K,V,T,O} <: AbstractPriorityQueue{K,V,T,O}
    order::O
    nodes::Vector{T}
    index::Dict{K,Int}
end

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

"""
struct FastPriorityQueue{V,T<:AbstractNode{Int,V},
                         O,N} <: AbstractPriorityQueue{Int,V,T,O}
    order::O
    nodes::Vector{T}
    index::Array{Int,N}
end

# Constructors for PriorityQueue instances.

function PriorityQueue{K,V}(o::O = FastMin,
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
    print(io, "priority queue of type ", nameof(typeof(pq)), "{", nameof(K),
          ",", nameof(V), "} with ", length(pq), " node(s)")

show(io::IO, ::MIME"text/plain", pq::FastPriorityQueue{V}) where {V} =
    print(io, "priority queue of type ", nameof(typeof(pq)), "{", nameof(V),
          "} with ", length(pq), " node(s)")

ordering(pq::AbstractPriorityQueue)  = getfield(pq, :order)
nodes(pq::AbstractPriorityQueue) = getfield(pq, :nodes)
index(pq::AbstractPriorityQueue) = getfield(pq, :index)

haskey(pq::PriorityQueue, key) = haskey(index(pq), key)
haskey(pq::FastPriorityQueue, key...) = (index(pq)[key...] > 0)

length(pq::AbstractPriorityQueue) = length(nodes(pq))
isempty(pq::AbstractPriorityQueue) = (length(pq) ≤ 0)

first(pq::AbstractPriorityQueue) = peek(pq)

# FIXME: Same code as for binary heaps.
function peek(pq::AbstractPriorityQueue)
    isempty(pq) && throw_argument_error("priority queue is empty")
    @inbounds x = getindex(nodes(pq), 1)
    return Tuple(x)
end

function empty!(pq::AbstractPriorityQueue; slow::Bool=false)
    if slow || length(pq) > 0
        force_empty!(pq)
    end
    return pq
end

function force_empty!(pq::PriorityQueue)
    empty!(nodes(pq))
    empty!(index(pq))
end

function force_empty!(pq::FastPriorityQueue)
    empty!(nodes(pq))
    fill!(index(pq), 0)
end

pop!(pq::AbstractPriorityQueue) = dequeue!(pq)

# This is almost the same code as pop! for a binary heap.
function dequeue!(pq::AbstractPriorityQueue)
    n = length(pq)
    n ≥ 1 || throw_argument_error("priority queue is empty")
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
    return Tuple(x)
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

push!(pq::AbstractPriorityQueue{<:Any,<:Any,T}, node::T) where {T}=
    enqueue!(pq, node)

getindex(pq::AbstractPriorityQueue, ::Tuple{}) = throw_missing_key()

setindex!(pq::AbstractPriorityQueue, val, ::Tuple{}) = throw_missing_key()

throw_missing_key() = throw_argument_error("missing key")

getindex(pq::PriorityQueue, key) = getval(pq, get(index(pq), key, 0))

setindex!(pq::PriorityQueue, val, key) = enqueue!(pq, key, val)

@inline function getindex(pq::FastPriorityQueue,
                          key::Union{Integer, CartesianIndex}...)
    I = index(pq)
    k = to_indices(I, key)
    @boundscheck checkbounds(I, k...)
    @inbounds i = I[k...]
    return getval(pq, i)
end

@inline function setindex!(pq::FastPriorityQueue, val,
                           key::Union{Integer, CartesianIndex}...)
    return enqueue!(pq, key, val)
end

to_key(pq::AbstractPriorityQueue{K,V}, key) where {K,V} = convert(K, key)
to_val(pq::AbstractPriorityQueue{K,V}, val) where {K,V} = convert(V, val)
to_node(pq::AbstractPriorityQueue{K,V,T}, key, val) where {K,V,T} =
    T(to_key(pq, key), to_val(pq, val))

# For fast priority queues, to_key shall yield a linear index.  The key can be
# a linear index or a multi-dimensional index (anything accepted by
# to_indices).  In the former case, we have to check whether the linear index
# is in bounds.  In the latter case, we use LinearIndices which does the bounds
# checking and this cannot be avoided.

@inline @propagate_inbounds to_key(pq::FastPriorityQueue, key::Integer) =
    # Convert to Int, then re-call to_key for bound checking.  Note that type
    # assertion neded to avoid infite recursion.
    to_key(pq, convert(Int, key)::Int)

@inline function to_key(pq::FastPriorityQueue, key::Int)
    @boundscheck checkbounds(index(pq), key)
    return key
end

@inline function to_key(pq::FastPriorityQueue,
                        key::Tuple{Vararg{Union{Integer,CartesianIndex}}})
    I = index(pq)
    k = to_indices(I, key)
    isa(k, Dims{ndims(I)}) || throw_invalid_key(pq, key)
    # FIXME: Shall we store the linear_indices (a small object) in the priority
    #        queue directly?
    return LinearIndices(I)[k...]
end

to_key(pq::FastPriorityQueue, key) = throw_invalid_key(pq, key)

@noinline throw_invalid_key(pq::AbstractPriorityQueue, key) = throw_argument_error(
    "invalid key of type ", typeof(key), " for ", nameof(typeof(pq)))

@noinline throw_invalid_key(pq::FastPriorityQueue, key) = throw_argument_error(
    "invalid key of type ", typeof(key), " for ", nameof(typeof(pq)),
    " expecting a linear index, an ", ndims(index(pq)),
    "-dimensional Cartesian index")

# The following is to allow the syntax enqueue!(pq, key=>val)
enqueue!(pq::AbstractPriorityQueue, pair::Pair) =
    enqueue!(pq, pair.first, pair.second)

enqueue!(pq::PriorityQueue, key, val) =
    enqueue!(pq, to_node(pq, key, val))

enqueue!(pq::PriorityQueue{K,V,T}, node::T) where {K,V,T} =
    unsafe_enqueue!(pq, node, get(index(pq), getkey(node), 0))

# At the first stage of the enqueue! method, avoiding bounds checking
# is not a good idea: it saves almost nothing due to the amount of work;
# if a multi-dimensional key is there are little interests in
function enqueue!(pq::FastPriorityQueue, key::Int, val)
    I = index(pq)
    in_range(key, I) || throw_argument_error("out of bounds key")
    @inbounds i = I[key]
    return unsafe_enqueue!(pq, to_node(pq, key, val), i)
end

function enqueue!(pq::FastPriorityQueue,
                  key::Tuple{Vararg{Union{Integer,CartesianIndex}}}, val)
    I = index(pq)
    k = to_indices(I, key)
    isa(k, Dims{ndims(I)}) || throw_invalid_key(pq, key)
    # FIXME: Shall we store the linear_indices (a small object) in the priority
    #        queue directly?
    l = LinearIndices(I)[k...]
    @inbounds i = I[l] # in principle l is correct
    return unsafe_enqueue!(pq, to_node(pq, l, val), i)
end

enqueue!(pq::FastPriorityQueue, key::Integer, val) =
    enqueue!(pq, convert(Int, key)::Int, val)

enqueue!(pq::FastPriorityQueue{V,T}, node::T) where {V,T} =
    enqueue!(pq, getkey(node), getval(node))

# The following unsafe method assumes that the key is valid (e.g. it is not
# out of bounds for fast priority queues) in the sense that I[key] is valid
# for the index I of the priority queue.
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

function delete!(pq::AbstractPriorityQueue, key)
    i = find_key(pq, key)
    A = nodes(pq)
    n = length(A)
    if in_range(i, A)
        if n > 1
            # Replace the deleted node by the last node in the heap.
            @inbounds x = A[n] # last node
            @inbounds y = A[i] # node to be deleted
            o = ordering(pq)
            if lt(o, y, x)
                # Heap structure _above_ deleted node is already valid.
                unsafe_heapify_down!(pq, i, x, n - 1)
            else
                # Heap structure _below_ deleted node is already valid.
                unsafe_heapify_up!(pq, i, x)
            end
            unsafe_delete_key!(pq, getkey(x))
            unsafe_shrink!(pq, n - 1)
        end
    end
    return pq
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
