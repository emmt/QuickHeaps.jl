"""
    has_standard_linear_indexing(A)

yields whether array `A` implements standard linear indexing (1-based).

"""
has_standard_linear_indexing(A::Array) = true
has_standard_linear_indexing(A::AbstractArray) =
    is_one_based_unit_range(eachindex(A))

"""
    is_one_based_unit_range(itr)

yields whether iterator `itr` is a 1-based unit range.

"""
is_one_based_unit_range(itr) = false
is_one_based_unit_range(itr::Base.OneTo) = true
is_one_based_unit_range(itr::AbstractUnitRange{T}) where {T} =
    first(itr) == oneunit(T)

"""
    to_eltype(A, x)

lazily yields `x` converted to the type of the elements of `A`.

"""
to_eltype(A, x) = to_type(eltype(A), x)

"""
    in_range(i, len::Integer)

yields whether `1 ≤ i ≤ len`.

    in_range(i, A::Array)

yields whether `i` is a valid linear index of array `A`.

    in_range(i, R::AbstractUnitRange{<:Integer})

yields whether `i` is in the range `R`.

"""
in_range(i::Integer, len::Integer) = ((i % UInt) - 1 < (len % UInt))
in_range(i::Integer, A::Array) = in_range(i, length(A))
in_range(i::Integer, R::OneTo) = in_range(i, length(R))
in_range(i::Integer, R::AbstractUnitRange{<:Integer}) = (i ∈ R)

"""
    has_bad_values(A[, isbad])

yields whether array `A` has bad values according to predicate `isbad`. For
arrays with floating-point values, `isbad` default to `isnan` if unspecified.
For integer-valued arrays, this function always returns `false` if `isnan` is
unspecified.

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
    typename(x)

yields a short string describing the type of object `x`.  Argument may also be
the object type.

"""
typename(x::Any) = typename(typeof(x))
typename(T::DataType) = string(nameof(T))

for (func, type) in ((:throw_argument_error, :ArgumentError),
                     (:throw_dimension_mismatch, :DimensionMismatch),)
    @eval begin
        $func(msg::$type.types[1]) = throw($type(msg))
        @noinline $func(args...) = $func(string(args...))
    end
end
