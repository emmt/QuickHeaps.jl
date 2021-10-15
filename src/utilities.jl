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
    to_type(T, x)

lazily yields `x` converted to type `T`.  If `x` is of type `T`, `x` is
returned; otherwise `convert(T, x)::T` is returned.  Thus the result is
guaranteed to be of type `T`.  This mimics what is done by `setindex!` for
arrays (see file `array.jl` in Julia base code).

"""
to_type(::Type{T}, x::T) where {T} = x
to_type(::Type{T}, x) where {T} = convert(T, x)::T

"""
    to_eltype(A, x)

lazily yields `x` converted to the type of the elements of `A`.

"""
to_eltype(A, x) = to_type(eltype(A), x)

"""
    to_int(x)

lazily yields integer `x` converted to type `Int`.

"""
to_int(x::Integer) = to_type(Int, x)

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
    has_bad_values(A, isbad)

yields whether array `A` has bad values according to predicate `isbad`.  For
arrays with floating-point values, `isbad` default to `isnan` if unspecified.

"""
function has_bad_values(A::AbstractArray, isbad)
    flag = false
    @inbounds @simd for i in eachindex(A)
        flag |= isbad(A[i])
    end
    return flag
end

has_bad_values(A::AbstractArray{<:Integer}) = false
has_bad_values(A::AbstractArray{<:AbstractFloat}) =
    has_bad_values(A, isnan)

require_one_based_indexing(A...) =
    has_offset_axes(A...) && throw_argument_error(
        "arrays must have 1-based indexing")

throw_argument_error(msg::AbstractString) = throw(ArgumentError(msg))
@noinline throw_argument_error(args...) =
    throw_argument_error(string(args...))

throw_dimension_mismatch(msg::AbstractString) = throw(DimensionMismatch(msg))
@noinline throw_dimension_mismatch(args...) =
    throw_dimension_mismatch(string(args...))
