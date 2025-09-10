const EMPTY_PRIORITY_QUEUE_ERROR = ArgumentError("priority queue is empty")

"""
    PriorityQueue{K,V}(o=TotalMin)

yields a priority queue for keys of type `K` and priority values of type `V`. Optional
argument `o::Ordering` specifies the ordering of values.

If keys are analogous to array indices (linear or Cartesian), [`FastPriorityQueue`](@ref)
may provide a faster alternative.

"""
function PriorityQueue{K,V}(o::O = default_ordering(PriorityQueue)) where {K,V,O<:Ordering}
    return PriorityQueue(o, Pair{K,V}[], Dict{K,Int}())
end

"""
    FastPriorityQueue{V}(o=TotalMin, dims...)

yields a priority queue for keys analogous of indices in an array of size `dims...` and
priority values of type `V`. Optional argument `o::Ordering` specifies the ordering of
values. The keys are stored as linear indices of type `Int`.

See [`PriorityQueue`](@ref) if keys cannot be assumed to be array indices.

""" FastPriorityQueue

# Constructors of FastPriorityQueue instances.

FastPriorityQueue{V}(dims::Integer...) where {V} =
    FastPriorityQueue{V}(dims)

FastPriorityQueue{V}(dims::Tuple{Vararg{Integer}}) where {V} =
    FastPriorityQueue{V}(default_ordering(FastPriorityQueue), dims)

FastPriorityQueue{V}(o::Ordering, dims::Integer...) where {V} =
    FastPriorityQueue{V}(o, dims)

FastPriorityQueue{V}(o::Ordering, dims::Tuple{Vararg{Integer}}) where {V} =
    _FastPriorityQueue(o, Pair{Int,V}[], zeros(Int, dims))

# Copy constructors. The copy is independent from the original.
Base.copy(pq::PriorityQueue) =
    PriorityQueue(pq.order, copy(pq.pairs), copy(pq.index))

Base.copy(pq::FastPriorityQueue) =
    _FastPriorityQueue(pq.order, copy(pq.pairs), copy(pq.index))

Base.show(io::IO, ::MIME"text/plain", pq::PriorityQueue{K,V}) where {K,V} =
    print(io, "priority queue of type ", nameof(typeof(pq)), "{", nameof(K),
          ",", nameof(V), "} with ", length(pq), " entry(s)")

Base.show(io::IO, ::MIME"text/plain", pq::FastPriorityQueue{V}) where {V} =
    print(io, "fast priority queue of  of type ", nameof(typeof(pq)), "{", nameof(V),
          "} with ", length(pq), " entry(s)")

Base.Order.Ordering(pq::Union{PriorityQueue,FastPriorityQueue}) = getfield(pq, :order)

# Implement abstract dictionary API for priority queues. NOTE `keytype` and `valtype` are
# already provided for any type inheriting from `AbstractDict`.

Base.length(pq::AbstractPriorityQueue) = length(pq.pairs)
Base.isempty(pq::AbstractPriorityQueue) = length(pq) < 1
Base.haskey(pq::AbstractPriorityQueue, key) = heap_index(pq, key) > 0

get_key(x::Pair) = x.first
get_val(x::Pair) = x.second

for (func, getter) in ((:get, :get_val), (:getkey, :get_key))
    @eval function Base.$func(pq::AbstractPriorityQueue, key, def)
        i = heap_index(pq, key)
        return i > 0 ? $getter(@inbounds pq.pairs[i]) : def
    end
end

function Base.delete!(pq::AbstractPriorityQueue, key)
    n = length(pq)
    if n > 0
        i = heap_index(pq, key)
        if i > 0
            A = pq.pairs
            key = get_key(@inbounds A[i]) # key to be deleted
            if i < n
                # NOTE We cannot assume that the deleted entry data be accessible nor valid,
                #      so we explicitly replace it before deciding in which direction to go
                #      and re-heapify. Also see `unsafe_enqueue!`.
                #
                # Replace the deleted entry in the heap by the last entry.
                x = @inbounds A[n]
                @inbounds A[i] = x
                # Up-/down-heapify to restore the binary heap structure.
                o = pq.order
                if i ≤ 1 || lt(o, get_val(@inbounds A[heap_parent(i)]), get_val(x))
                    unsafe_heapify_down!(pq, i, x, n - 1)
                else
                    unsafe_heapify_up!(pq, i, x)
                end
            end
            unsafe_shrink!(pq, n - 1)
            unsafe_delete_key!(pq, key)
        end
    end
    return pq
end

Base.first(pq::AbstractPriorityQueue) = peek(pq)

# NOTE `QuickHeaps.peek` is `Base.peek` if this symbol is defined in base Julia.
function peek(pq::AbstractPriorityQueue)
    isempty(pq) && throw(EMPTY_PRIORITY_QUEUE_ERROR)
    return @inbounds pq.pairs[1]
end

function Base.empty!(pq::PriorityQueue)
    empty!(pq.pairs)
    empty!(pq.index)
    return pq
end

function Base.empty!(pq::FastPriorityQueue)
    empty!(pq.pairs)
    fill!(pq.index, 0)
    return pq
end

# Private structure used by iterators on priority queues.
struct PriorityQueueIterator{F,Q<:AbstractPriorityQueue}
    getter::F
    parent::Q
end

Base.parent(itr::PriorityQueueIterator) = getfield(itr, :parent)

Base.IteratorEltype(itr::PriorityQueueIterator) = Base.IteratorEltype(typeof(itr))
Base.IteratorEltype(::Type{<:PriorityQueueIterator}) = Base.HasEltype()
Base.eltype(itr::PriorityQueueIterator) = eltype(typeof(itr))
Base.eltype(::Type{<:PriorityQueueIterator{typeof(get_key),Q}}) where {Q} = keytype(Q)
Base.eltype(::Type{<:PriorityQueueIterator{typeof(get_val),Q}}) where {Q} = valtype(Q)
Base.eltype(::Type{<:PriorityQueueIterator{F,Q}}) where {F,Q} = Any

Base.IteratorSize(itr::PriorityQueueIterator) = Base.IteratorSize(typeof(itr))
Base.IteratorSize(::Type{<:PriorityQueueIterator}) = Base.HasLength()
Base.length(itr::PriorityQueueIterator) = length(parent(itr))

# Unordered iterators. NOTE: All iterators shall however return the elements in the same
# order.
function Base.iterate(pq::AbstractPriorityQueue, i::Int = 1)
    1 ≤ i ≤ length(pq) || return nothing
    return (@inbounds pq.pairs[i]), i + 1
end
Base.keys(pq::AbstractPriorityQueue) = PriorityQueueIterator(get_key, pq)
Base.values(pq::AbstractPriorityQueue) = PriorityQueueIterator(get_val, pq)
function Base.iterate(itr::PriorityQueueIterator, i::Int = 1)
    pq = itr.parent
    1 ≤ i ≤ length(pq) || return nothing
    return itr.getter(@inbounds pq.pairs[i]), i + 1
end

"""
    dequeue!(pq) -> key

Remove the root entry from the priority queue `pq` and return its key.

You may call [`dequeue_pair!(pq)`](@ref dequeue_pair!) to dequeue the root entry as a
key-value pair.

"""
dequeue!(pq::AbstractPriorityQueue) = get_key(dequeue_pair!(pq))

"""
    dequeue_pair!(pq) -> (key => val)

Remove the root entry from the priority queue `pq` and return it as a key-value pair. This
is the same as `pop!(pq)`.

Also see [`dequeue!`](@ref).

"""
function dequeue_pair!(pq::AbstractPriorityQueue)
    # The code is almost the same as pop! for a binary heap.
    n = length(pq)
    n > 0 || throw(EMPTY_PRIORITY_QUEUE_ERROR)
    A = pq.pairs
    x = @inbounds A[1] # get root entry
    if n > 1
        # Peek the last entry and down-heapify starting at the root of the binary heap to
        # insert it.
        unsafe_heapify_down!(pq, 1, (@inbounds A[n]), n - 1)
    end
    unsafe_delete_key!(pq, get_key(x))
    unsafe_shrink!(pq, n - 1)
    return x
end

# Implement `pop!` as for any dictionary.
Base.pop!(pq::AbstractPriorityQueue) = dequeue_pair!(pq)

# Implement `push!` for priority queues. NOTE Multi-push is already implemented for any
# collection; for `AbstractDict`, pushing pair(s) via `setindex!` is also already
# implemented.
Base.push!(pq::AbstractPriorityQueue, x::Pair) = enqueue!(pq, x)

"""
    pq[key...] = val
    setindex!(pq, val, key...) -> pq
    enqueue!(pq, key, val) -> pq
    enqueue!(pq, key => val) -> pq
    push!(pq, key => val) -> pq

Set the value `val` stored by the priority queue `pq` at index `key` automatically
maintaining the partial ordering of `pq`.

"""
enqueue!(pq::AbstractPriorityQueue, key, val) = setindex!(pq, val, key)
enqueue!(pq::AbstractPriorityQueue, (key, val)::Pair) = enqueue!(pq, key, val)

# `getindex` for priority queues.
function Base.getindex(pq::PriorityQueue, key)
    i = heap_index(pq, key)
    i > 0 || throw(KeyError(key))
    return get_val(@inbounds pq.pairs[i])
end

# `setindex!` for priority queues. We first convert the key and the value to their
# respective types because this is how they will be stored anyway.
Base.setindex!(pq::PriorityQueue{K,V}, val, key) where {K,V} =
    setindex!(pq, convert(V, val)::V, convert(K, key)::K)
Base.setindex!(pq::PriorityQueue{K,V}, val::V, key::K) where {K,V} =
    unsafe_enqueue!(pq, key => val, heap_index(pq, key))

# `getindex` for fast priority queues.
#
Base.getindex(pq::FastPriorityQueue, key) = throw(KeyError(key))
#
@propagate_inbounds Base.getindex(pq::FastPriorityQueue, key::Integer) =
    getindex(pq, Int(key)::Int)
#
@inline function Base.getindex(pq::FastPriorityQueue, key::Int)
    I = pq.index
    @boundscheck checkbounds(I, key)
    i = @inbounds I[key] # index in heap
    A = pq.pairs # the heap
    1 ≤ i ≤ length(A) || throw(KeyError(key))
    return get_val(@inbounds A[i])
end
#
@propagate_inbounds function Base.getindex(pq::FastPriorityQueue,
                                           key::Vararg{Union{Integer,CartesianIndex}})
    return getindex(pq, normalize_indices(key)...)
end
#
@inline function Base.getindex(pq::FastPriorityQueue{V,N},
                               key::Vararg{Int,N}) where {V,N}
    I = pq.index
    @boundscheck checkbounds(I, key...)
    i = @inbounds I[key...] # index in heap
    A = pq.pairs # the heap
    1 ≤ i ≤ length(A) || throw(KeyError(key))
    return get_val(@inbounds A[i])
end

# `setindex!` for fast priority queues.
#
Base.setindex!(pq::FastPriorityQueue, val, key) = throw(KeyError(key))
#
@propagate_inbounds function Base.setindex!(pq::FastPriorityQueue{V}, val,
                                            key::Integer) where {V}
    return setindex!(pq, convert(V, val)::V, Int(key)::Int)
end
#
@inline function Base.setindex!(pq::FastPriorityQueue{V}, val::V, key::Int) where {V}
    I = pq.index
    @boundscheck checkbounds(I, key)
    return unsafe_enqueue!(pq, key => val, (@inbounds I[key]))
end
#
@propagate_inbounds function Base.setindex!(pq::FastPriorityQueue{V}, val,
                                            key::Vararg{Union{Integer,CartesianIndex}}) where {V}
    return setindex!(pq, convert(V, val)::V, normalize_indices(key)...)
end
#
@inline function Base.setindex!(pq::FastPriorityQueue{V,N}, val::V,
                                key::Vararg{Int,N}) where {V,N}
    I = pq.index
    @boundscheck checkbounds(I, key...)
    idx = @inbounds linear_index(I, key...)
    return unsafe_enqueue!(pq, idx => val, (@inbounds I[idx]))
end

"""
    QuickHeaps.linearIndex(A, inds...) -> i::Int

Return the linear index `i` of the array element `A[inds...]` propagating the current bound
checking state.

"""
@propagate_inbounds function linear_index(A::AbstractArray,
                                          inds::Vararg{Union{Integer,CartesianIndex}})
    return LinearIndices(A)[inds...]
end

# In a "fast priority queue", a key, so called a "fast key", is the analogous of
# the array index of a single entry in `getindex` or `setindex!` functions before the
# processing by Julia. A "fast key" can be an integer, a Cartesian index, or a tuple of these
# (including a mixture). `normalize_indices(key)` yields a single linear index (an `Int`)
# if the key is an integer or a tuple of `Int`s otherwise. It is similar to `to_indices` but
# much simpler because things such as ranges are not allowed in the "fast key".
#
normalize_indices(key::Integer) = Int(key)::Int
normalize_indices(key::CartesianIndex) = Tuple(key)::Tuple{Vararg{Int}}
#
# For a tuple of integers and/or Cartesian indices, enter a recursive build of the result as
# a tuple of Int's.
@inline normalize_indices(key::Tuple{Vararg{Union{Integer,CartesianIndex}}}) =
    normalize_indices((), key)::Tuple{Vararg{Int}}
#
# Terminate the recursion.
@inline normalize_indices(inds::Tuple{Vararg{Int}}, key::Tuple{}) = inds
#
# Skip empty Cartesian indices.
@inline normalize_indices(inds::Tuple{Vararg{Int}}, key::Tuple{CartesianIndex{0}, Vararg}) =
    normalize_indices(inds, tail(key))
#
# Append Cartesian indices.
@inline normalize_indices(inds::Tuple{Vararg{Int}}, key::Tuple{CartesianIndex, Vararg}) =
    normalize_indices((inds..., Tuple(first(key))...), tail(key))
#
# Append single index.
@inline normalize_indices(inds::Tuple{Vararg{Int}}, key::Tuple{Integer, Vararg}) =
    normalize_indices((inds..., Int(first(key))::Int), tail(key))
#
# Error catcher.
@noinline normalize_indices(key) = throw_argument_error(
    "invalid fast key of type ", typeof(key))

"""
    QuickHeaps.heap_index(pq, key) -> i::Int

Return the index `i` corresponding to `key` in the binary heap backing the storage of the
entries of the priority queue `pq`. If the `key` is not in priority queue, `i = 0` is
returned, otherwise `i ∈ 1:n` with `n = length(pq)` is returned.

The `heap_index` method is used to implement `haskey`, `get`, and `delete!` methods for
priority queues. The `heap_index` method shall be specialized for any concrete sub-types of
`QuickHeaps.AbstractPriorityQueue`.

""" heap_index # NOTE: `heap_index` is like `ht_keyindex` in `base/dict.jl`

heap_index(pq::PriorityQueue, key) = get(pq.index, key, 0)
heap_index(pq::FastPriorityQueue, key) = isempty(pq) ? 0 : _heap_index(pq, key)

_heap_index(pq::FastPriorityQueue, key) = 0 # pretend that the key is missing by default
_heap_index(pq::FastPriorityQueue, key::Integer) = _heap_index(pq, Int(key)::Int)
function _heap_index(pq::FastPriorityQueue, key::Int)
    I = pq.index
    return checkbounds(Bool, I, key) ? (@inbounds I[key]) : 0
end
_heap_index(pq::FastPriorityQueue{V,N}, key::CartesianIndex{N}) where {V,N} =
    _heap_index(pq, Tuple(key)::NTuple{N,Int}...)
function _heap_index(pq::FastPriorityQueue{V,N}, key::Vararg{Int,N}) where {V,N}
    I = pq.index
    return checkbounds(Bool, I, key...) ? (@inbounds I[key...]) : 0
end

"""
    QuickHeaps.unsafe_enqueue!(pq, x, i) -> pq

Store `x = key=>val` in priority queue `pq` at index `i` and return the priority queue.

Argument `i` is an index in the binary heap backing the storage of the entries of the
priority queue. Index `i` shall have been determined according to `key` for `pq`. If `i` is
not a valid index in the binary heap, a new node is added; otherwise, the node at index `i`
in the binary heap is replaced by `x`. In any cases, the binary heap is reordered as needed.

This function is *unsafe* because it assumes that the `key` and `i` are consistent valid
(e.g. it is not out of bounds for fast priority queues) in the sense that `I[key]` is valid
for the index `I` of the priority queue.

"""
function unsafe_enqueue!(pq::AbstractPriorityQueue, x::Pair, i::Int)
    A = pq.pairs # heap of key=>val pairs
    n = length(A)
    if 1 ≤ i ≤ n
        # The key already exists. Replace the node in the heap by the new node and
        # up-/down-heapify to restore the binary heap structure. We cannot assume that the
        # replaced node data be accessible nor valid, so we explicitly replace it before
        # deciding in which direction to go and re-heapify. Also see `delete!`.
        @inbounds A[i] = x # replace deleted node
        o = pq.order
        if i ≤ 1 || lt(o, get_val(@inbounds A[heap_parent(i)]), get_val(x))
            unsafe_heapify_down!(pq, i, x)
        else
            unsafe_heapify_up!(pq, i, x)
        end
    else
        # No such key already exists. Create a new slot at the end of the node list and
        # up-heapify to fix the structure and insert the new node.
        unsafe_heapify_up!(unsafe_grow!(pq, n+1), n+1, x)
    end
    return pq
end

"""
    QuickHeaps.unsafe_grow!(pq, n) -> pq

grows the size of the binary heap backing the storage of the entries of the priority queue
`pq` to be `n` and returns the priority queue object.

"""
function unsafe_grow!(pq::Union{PriorityQueue,FastPriorityQueue}, n::Int)
    resize!(pq.pairs, n)
    return pq
end

"""
    QuickHeaps.unsafe_shrink!(pq, n)

shrinks the size of the binary heap backing the storage of the entries of the priority queue
`pq` to be `n`.

"""
unsafe_shrink!(pq::Union{PriorityQueue,FastPriorityQueue}, n::Int) =
    resize!(pq.pairs, n)

"""
    QuickHeaps.unsafe_delete_key!(pq, k)

deletes key `k` from the index of the priority queue `pq` assuming `k` is valid.

"""
unsafe_delete_key!(pq::AbstractPriorityQueue{K}, key::K) where {K} =
    unsafe_delete_key!(pq.index, key)

# Specialized version for the type of the index.
unsafe_delete_key!(I::Array{Int}, key::Int) = @inbounds I[key] = 0
unsafe_delete_key!(I::AbstractDict, key) = delete!(I, key)

@inline function unsafe_heapify_down!(pq::AbstractPriorityQueue, i::Int, x,
                                      n::Int = length(pq))
    o = pq.order
    A = pq.pairs
    I = pq.index
    @inbounds begin
        while (l = heap_left(i)) ≤ n
            j = (r = heap_right(i)) > n || lt(o, get_val(A[l]), get_val(A[r])) ? l : r
            lt(o, get_val(A[j]), get_val(x)) || break
            I[get_key(A[j])] = i
            A[i] = A[j]
            i = j
        end
        I[get_key(x)] = i
        A[i] = x
    end
end

@inline function unsafe_heapify_up!(pq::AbstractPriorityQueue, i::Int, x)
    o = pq.order
    A = pq.pairs
    I = pq.index
    @inbounds begin
        while (j = heap_parent(i)) ≥ 1 && lt(o, get_val(x), get_val(A[j]))
            I[get_key(A[j])] = i
            A[i] = A[j]
            i = j
        end
        I[get_key(x)] = i
        A[i] = x
    end
end
