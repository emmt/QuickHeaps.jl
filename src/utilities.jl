"""
    QuickHeaps.lt(o::Ordering, x::T, y::T)

yields whether `x` is less than `y` according to ordering `o`. This function is called by
`QuickHeaps` to build ordered structures like binary heaps and priority queues. For `T <:
QuickHeaps.Node`, it compares the values of the nodes `x` and `y`. It may be specialized for
custom node type `T`. By default, it calls `Base.Order.lt` to compare values.

""" lt

# Call Base.Order.lt by default.
lt(o::Ordering, x::T, y::T) where {T} = Base.Order.lt(o, x, y)

# Nodes are sorted according to their values.
lt(o::Ordering, x::T, y::T) where {T<:AbstractNode} = lt(o, get_val(x), get_val(y))

# In order to perform fast sorting (without taking care of NaN's), we extend the
# `Base.Order.lt` function for specialized ordering types.
Base.Order.lt(::FastForwardOrdering, x, y) = x < y

"""
    QuickHeaps.ordering(A) -> o

Return the object `o` specifying the ordering of the values in the object `A`, a binary heap
or a priority queue.

This method may be specialized for custom types.

"""
ordering(h::AbstractBinaryHeap) = getfield(h, :order)
ordering(pq::AbstractPriorityQueue) = getfield(pq, :order)

"""
    QuickHeaps.default_ordering(A)
    QuickHeaps.default_ordering(typeof(A))

yield the default ordering for the ordered data structure `A`. By default, this function
yields `QuickHeaps.SafeMin` which is an alias to `Base.Order.Forward`. This method may be
specialized for specific ordered data structure.

"""
default_ordering(x::Any) = default_ordering(typeof(x))
default_ordering(::Type) = SafeMin
default_ordering(::Type{<:AbstractBinaryHeap}) = SafeMin
default_ordering(::Type{<:FastBinaryHeap}) = FastMin
default_ordering(::Type{<:AbstractPriorityQueue}) = SafeMin

"""
    QuickHeaps.has_standard_linear_indexing(A)

yields whether array `A` implements standard linear indexing (1-based).

"""
has_standard_linear_indexing(A::Array) = true
has_standard_linear_indexing(A::AbstractArray) =
    is_one_based_unit_range(eachindex(A))

"""
    QuickHeaps.is_one_based_unit_range(itr)

yields whether iterator `itr` is a 1-based unit range.

"""
is_one_based_unit_range(itr) = false
is_one_based_unit_range(itr::Base.OneTo) = true
is_one_based_unit_range(itr::AbstractUnitRange{T}) where {T} =
    first(itr) == oneunit(T)

"""
    QuickHeaps.in_range(i, len::Integer)

yields whether `1 ≤ i ≤ len`.

    QuickHeaps.in_range(i, A::Array)

yields whether `i` is a valid linear index of array `A`.

    QuickHeaps.in_range(i, R::AbstractUnitRange{<:Integer})

yields whether `i` is in the range `R`.

"""
in_range(i::Integer, len::Integer) = ((i % UInt) - 1 < (len % UInt))
in_range(i::Integer, A::Array) = in_range(i, length(A))
in_range(i::Integer, R::OneTo) = in_range(i, length(R))
in_range(i::Integer, R::AbstractUnitRange{<:Integer}) = (i ∈ R)

"""
    QuickHeaps.has_bad_values(A[, isbad])

yields whether array `A` has bad values according to predicate `isbad`. For arrays with
floating-point values, `isbad` default to `isnan` if unspecified. For integer-valued arrays,
this function always returns `false` if `isnan` is unspecified.

"""
function has_bad_values(A::AbstractArray, isbad)
    flag = false
    @inbounds @simd for i in eachindex(A)
        flag |= isbad(A[i])
    end
    return flag
end
has_bad_values(A::AbstractArray{<:Integer}) = false
has_bad_values(A::AbstractArray{<:AbstractFloat}) = has_bad_values(A, isnan)

"""
    QuickHeaps.typename(x)
    QuickHeaps.typename(typeof(x))

yield a short string describing the type of object `x`.

"""
typename(x::Any) = typename(typeof(x))
typename(T::DataType) = string(nameof(T))
typename(::Type{<:AbstractBinaryHeap}) = "binary heap"
typename(::Type{<:AbstractPriorityQueue}) = "priority queue"

for (func, type) in ((:throw_argument_error, :ArgumentError),
                     (:throw_dimension_mismatch, :DimensionMismatch),)
    @eval begin
        $func(msg::$type.types[1]) = throw($type(msg))
        @noinline $func(args...) = $func(string(args...))
    end
end
