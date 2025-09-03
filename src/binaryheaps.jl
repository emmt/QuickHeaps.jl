"""
    h = BinaryHeap{T}([o::Base.Order.Ordering = TotalMin,][ vals::AbstractVector])

Build an empty binary heap whose values have type `T` and with ordering specified by `o`.

The method [`QuickHeaps.lt(o,x::T,y::T)`](@ref QuickHeaps.lt) is called to determine the
order of values `x` and `y` in the heap. The default ordering, [`TotalMin`](@ref), yields a
*min-heap* object; with [`TotalMax`](@ref) ordering, a *max-heap* object is returned.

An optional vector `vals` storing the initial values of the binary heap can be specified.
These values in `vals` need not be ordered, the `BinaryHeap` constructor automatically takes
care of that. If `vals` is a `Vector{T}` instance, the binary-heap will be directly built
into `vals`. Call `BinaryHeap(copy(vals))` to create a binary heap with its own storage.

Arguments `o` and `vals` may be specified in any order.

Method `sizehint!(h,n)` may be called to anticipate that the heap may contains `n` values.

""" BinaryHeap

"""
    h = FastBinaryHeap{T}([o::Base.Order.Ordering = TotalMin,][ vals::AbstractVector])

Build a fast binary heap. Compared to [`BinaryHeap{T}(...)`](@ref BinaryHeap), the array
backing the storage of the heap values is never automatically reduced to improve
performances in some cases. You may call `resize!(h)` to explicitly reduce the storage of
fast binary-heap `h` to its minimum.

""" FastBinaryHeap

# Outer constructors.
for type in (:BinaryHeap, :FastBinaryHeap)
    @eval begin
        $type{T}(vals::AbstractVector, o::Ordering=default_ordering($type)) where {T} =
            $type{T}(o, vals)
        $type(vals::AbstractVector{T}) where {T} = $type{T}(vals)
        $type(o::Ordering, vals::AbstractVector{T}) where {T} = $type{T}(o, vals)
        $type(vals::AbstractVector{T}, o::Ordering) where {T} = $type{T}(o, vals)
    end
end

"""
    QuickHeaps.storage(h)

yields the array backing the storage of the values in the binary heap `h`.

This method may be specialized for custom binary heap types.

"""
storage(h::AbstractBinaryHeap) = getfield(h, :data)

Base.length(h::FastBinaryHeap) = getfield(h, :count)
Base.length(h::BinaryHeap) = length(storage(h))
Base.size(h::AbstractBinaryHeap) = (length(h),)
Base.IndexStyle(::Type{<:AbstractBinaryHeap}) = IndexLinear()
function Base.sizehint!(h::AbstractBinaryHeap, n::Integer)
    sizehint!(storage(h), n)
    return h
end
Base.isempty(h::AbstractBinaryHeap) = length(h) < 1

# Call `resize!(h)` with no other arguments to reduce the storage size.
Base.resize!(h::AbstractBinaryHeap) = h # do nothing by default
function Base.resize!(h::FastBinaryHeap)
    resize!(storage(h), length(h))
    return h
end

# Heap indexing. Note that linear 1-based indexing is assumed for the array storing the
# heap.
heap_left(i::Int) = 2*i
heap_right(i::Int) = 2*i + 1
heap_parent(i::Int) = div(i, 2)

@inline function Base.getindex(h::AbstractBinaryHeap, i::Int)
    @boundscheck checkbounds(h, i)
    return @inbounds storage(h)[i]
end

@propagate_inbounds Base.setindex!(h::AbstractBinaryHeap, x, i::Int) =
    setindex!(h, as(eltype(h), x), i)

@inline function Base.setindex!(h::AbstractBinaryHeap{T}, x::T, i::Int) where {T}
    @boundscheck checkbounds(h, i)
    A = storage(h)
    y = @inbounds A[i] # replaced value
    o = ordering(h)
    if lt(o, y, x)
        # Heap structure _above_ replaced entry will remain valid, down-heapify to fix the
        # heap structure at and _below_ the entry.
        unsafe_heapify_down!(o, A, i, x, length(h))
    else
        # Heap structure _below_ replaced entry will remain valid, up-heapify to fix the
        # heap structure at and _above_ the entry.
        unsafe_heapify_up!(o, A, i, x)
    end
    return h
end

Base.first(h::AbstractBinaryHeap) = peek(h)

# NOTE `QuickHeaps.peek` is `Base.peek` if this symbol is defined in base Julia.
function peek(h::AbstractBinaryHeap)
    isempty(h) && throw_argument_error(typename(h), " is empty")
    return @inbounds storage(h)[1]
end

Base.empty!(h::FastBinaryHeap) = (setfield!(h, :count, 0); h)
Base.empty!(h::BinaryHeap) = (empty!(storage(h)); h)

function Base.pop!(h::AbstractBinaryHeap)
    n = length(h)
    n ≥ 1 || throw_argument_error(typename(h), " is empty")
    A = storage(h)
    x = @inbounds A[1]
    if n > 1
        # Peek the last value and down-heapify starting at the root of the binary heap to
        # insert it.
        y = @inbounds A[n]
        unsafe_heapify_down!(ordering(h), A, 1, y, n - 1)
    end
    unsafe_shrink!(h, n - 1)
    return x
end

# Implement `push!` for binary heaps. NOTE Multi-push is already implemented for any
# collection.
Base.push!(h::AbstractBinaryHeap, x) = push!(h, as(eltype(h), x))
function Base.push!(h::AbstractBinaryHeap{T}, x::T) where {T}
    n = length(h) + 1
    unsafe_heapify_up!(ordering(h), unsafe_grow!(h, n), n, x)
    return h
end

"""
    setroot!(h, x) -> h

replaces the value of the root note in heap `h` by `x`. This is similar to `h[1] = x` but a
bit faster.

"""
setroot!(h::AbstractBinaryHeap, x) = setroot!(h, as(eltype(h), x))

function setroot!(h::AbstractBinaryHeap{T}, x::T) where {T}
    n = length(h)
    n ≥ 1 || throw_argument_error(typename(h), " is empty")
    unsafe_heapify_down!(ordering(h), storage(h), 1, x, n)
    return h
end

Base.delete!(h::AbstractBinaryHeap, i::Integer) = delete!(h, as(Int, i))

function Base.delete!(h::AbstractBinaryHeap, i::Int)
    n = length(h)
    in_range(i, n) || throw_argument_error("out of range index")
    if i < n
        # Replace the deleted value by the last value in the heap and up-/down-heapify to
        # restore the binary heap structure.
        A = storage(h)
        o = ordering(h)
        x = @inbounds A[n] # value to replace deleted value
        y = @inbounds A[i] # deleted value
        if lt(o, y, x)
            # Heap structure _above_ deleted entry will remain valid, down-heapify to fix
            # the heap structure at and _below_ the entry.
            unsafe_heapify_down!(o, A, i, x, n - 1)
        else
            # Heap structure _below_ deleted entry will remain valid, up-heapify to fix the
            # heap structure at and _above_ the entry.
            unsafe_heapify_up!(o, A, i, x)
        end
    end
    unsafe_shrink!(h, n - 1)
    return h
end

"""
    QuickHeaps.unsafe_grow!(h, n) -> A

grows the size of the binary heap `h` to be `n` and returns the array `A` backing the
storage of the values. This method is *unsafe* because it does not check its arguments and
because it breaks the binary heap structure of the array of values.

This method is called by `push!` to grow the size of the heap and shall be specialized for
any concrete sub-types of `QuickHeaps.AbstractBinaryHeap`.

"""
unsafe_grow!(h::BinaryHeap, n::Int) = resize!(storage(h), n)
unsafe_grow!(h::FastBinaryHeap, n::Int) = begin
    A = storage(h)
    length(A) < n && resize!(A, n)
    setfield!(h, :count, n)
    return A
end

"""
    QuickHeaps.unsafe_shrink!(h, n)

shrinks the size of the binary heap `h` to be `n`. This method is *unsafe* because it does
not check its arguments.

This method is called by `delete!` to eventually reduce the size of the heap and shall be
specialized for any concrete sub-type of [`QuickHeaps.AbstractBinaryHeap`](@ref).

"""
unsafe_shrink!(h::BinaryHeap, n::Int) = resize!(storage(h), n)
unsafe_shrink!(h::FastBinaryHeap, n::Int) = setfield!(h, :count, n)

"""
    heapify!(h) -> h

reorders the values in the binary heap `h` in-place. This method should be called to
initialize the heap or to re-order the heap if its contents have been modified by other
methods than `pop!` or `push!`.

The method can be called at a lower level to heapify (part of) an array storing the heap
values:

    heapify!([o=TotalMin,] A, n=length(A)) -> A

reorders the `n` first elements of array `A` in-place to form a binary heap according to the
ordering specified by `o`. The array `A` must have 1-based linear indexing. Arguments may be
specified in any order.

"""
function heapify!(h::AbstractBinaryHeap)
    heapify!(ordering(h), storage(h), length(h))
    return h
end

heapify!(o::Ordering, A::AbstractArray, n::Integer) = heapify!(o, A, as(Int, n))

function heapify!(o::Ordering, A::AbstractArray, n::Int = length(A))
    # Heapify the n first elements of A.
    check_heap_storage(A, n)
    @inbounds for i in heap_parent(n):-1:1
        unsafe_heapify_down!(o, A, i, A[i], n)
    end
    return A
end

"""
    heapify([o=TotalMin,] A, n=length(A))

yields an array with the `n` first values of array `A` stored in a binary heap structure of
ordering specified by `o`. The storage of the returned heap is a different array than `A`.
Arguments may be specified in any order.

"""
heapify(o::Ordering, A::AbstractArray, n::Integer) = heapify(o, A, as(Int, n))

heapify(o::Ordering, A::AbstractArray{T}, n::Int = length(A)) where {T} =
    heapify!(o, copyto!(Vector{T}(undef, n), 1, A, 1, n))

"""
    isheap([o=TotalMin,], A, n=length(A))

yields whether the `n` first elements of array `A` have a binary heap structure ordered as
specified by `o`. Arguments may be specified in any order.

    isheap(obj; check=false)

yields whether object `obj` is a binary heap. If keyword `check` is true, the internal
structure of `obj` is checked; otherwise, the type of `obj` is trusted to determine whether
it is a binary heap.

"""
isheap(o::Ordering, A::AbstractArray, n::Integer) = isheap(o, A, as(Int, n))

function isheap(o::Ordering, A::AbstractArray, n::Int = length(A))
    check_heap_storage(A, n)
    @inbounds for i in 1:div(n, 2)
        l = heap_left(i)
        r = heap_right(i)
        if lt(o, A[l], A[i]) || (r ≤ n && lt(o, A[r], A[i]))
            return false
        end
    end
    return true
end

isheap(h::AbstractBinaryHeap; check::Bool = false) =
    if check
        isheap(ordering(h), storage(h), length(h))
    else
        true
    end

# Cope with different ordering of arguments and using the same default ordering as in base
# Julia and DataStructures.
for func in (:heapify, :heapify!, :isheap)
    @eval begin
        function $func(A::AbstractArray,
                       o::Ordering = default_ordering(A),
                       n::Integer = length(A))
            return $func(o, A, n)
        end
        function $func(A::AbstractArray, n::Integer,
                       o::Ordering = default_ordering(A))
            return $func(o, A, n)
        end
    end
end

"""
    QuickHeaps.heapify_down!(o, A, i, x=A[i], n=lengh(A)) -> A

stores the value `x` in the `i`-th entry of the binary heap built into the `n` first
elements of array `A` with ordering `o` and, if needed, moves down the inserted value to
maintain the binary heap structure.

This method is called to *heapify* an array in order to initialize or rebuild the heap
structure or to replace the value of the root value of the heap and update the heap
structure.

"""
function heapify_down!(o::Ordering, A::AbstractArray,
                       i::Integer, x = A[i], n::Integer = length(A))
    heapify_down!(o, A, as(Int, i), as(eltype(A), x), as(Int, n))
end

function heapify_down!(o::Ordering, A::AbstractArray{T},
                       i::Int, x::T, n::Int) where {T}
    check_heap_storage(A, n)
    in_range(i, n) || throw_argument_error("out of range index")
    unsafe_heapify_down!(o, A, i, x, n)
    return A
end

"""
    QuickHeaps.unsafe_heapify_down!(o, A, i, x=A[i], n=lengh(A))

This method is a fast but *unsafe* version of [`QuickHeaps.heapify_down!`](@ref) which
assumes that all arguments are correct, that is `A` implements 1-based linear indexing, `0 ≤
n ≤ lengh(A)`, and `1 ≤ i ≤ n`.

"""
@inline function unsafe_heapify_down!(o::Ordering,
                                      A::AbstractArray{T},
                                      i::Int,
                                      x::T = (@inbounds A[i]),
                                      n::Int = length(A)) where {T}
    @inbounds begin
        while (l = heap_left(i)) ≤ n
            j = (r = heap_right(i)) > n || lt(o, A[l], A[r]) ? l : r
            lt(o, A[j], x) || break
            A[i] = A[j]
            i = j
        end
        A[i] = x
    end
end

"""
   QuickHeaps.heapify_up!(o, A, i, x=A[i]) -> A

stores the value `x` in the `i`-th entry of the binary heap built into the `i` first
elements of array `A` with ordering `o` and, if needed, moves up the value to maintain the
heap structure.

"""
function heapify_up!(o::Ordering, A::AbstractArray,
                     i::Integer, x = A[i])
    heapify_up!(o, A, as(Int, i), as(eltype(A), x))
end

function heapify_up!(o::Ordering, A::AbstractArray{T}, i::Int, x::T) where {T}
    check_heap_storage(A)
    in_range(i, length(A)) || error("out of range index")
    unsafe_heapify_up!(o, A, i, x)
    return A
end

"""
    QuickHeaps.unsafe_heapify_up!(o, A, i, x=A[i])

This methods is a fast but *unsafe* version of [`QuickHeaps.heapify_up!`](@ref) which
assumes that all arguments are correct, that is `A` implements 1-based linear indexing and
`1 ≤ i ≤ length(A)`.

"""
@inline function unsafe_heapify_up!(o::Ordering,
                                    A::AbstractArray{T},
                                    i::Int,
                                    x::T = (@inbounds A[i])) where {T}
    @inbounds begin
        while (j = heap_parent(i)) ≥ 1 && lt(o, x, A[j])
            A[i] = A[j]
            i = j
        end
        A[i] = x
    end
end

"""
    QuickHeaps.check_heap_storage(A)

throws an exception if array `A` is not suitable for storing a binary heap, that is if `A`
does not have 1-based linear indexing.

    QuickHeaps.check_heap_storage(A, n)

throws an exception if the first elements of array `A` are not suitable for storing a binary
heap of size `n`.

"""
function check_heap_storage(A::AbstractArray)
    has_standard_linear_indexing(A) || throw(ArgumentError(
        "array storing a binary heap must have 1-based linear indexing"))
    nothing
end

function check_heap_storage(A::AbstractArray, n::Int)
    # Check that array has linear indexing and that 0 ≤ n ≤ length(A).
    check_heap_storage(A)
    (n % UInt) ≤ (length(A) % UInt) || throw_argument_error(
        "out of range heap size")
    nothing
end
