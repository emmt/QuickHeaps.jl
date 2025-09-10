# Custom order

If the ordering rules provided by `QuickHeaps` are not suitable for your needs, you may
customize how to order the values in binary heaps or in priority queues.

## General order rules

In `QuickHeaps`, like in most Julia sorting algorithms, the order of entries
is fully determined by the `Base.Order.lt` function with the
following signature:

```julia
Base.Order.lt(o::OrderType, x::T, y::T) where {T<:ValueType}
```

which shall yield whether value `x` has (strictly) higher priority than value `y` and where
`OrderType <: Base.Order.Ordering` and `ValueType` are the respective types of the order and
of the values of the binary heap or priority queue. To implement a new specific order, the
`Base.Order.lt` can be specialized for `OrderType` and/or for `ValueType`. Of course, this
must be done while avoiding [type
piracy](https://docs.julialang.org/en/v1/manual/style-guide/#Avoid-type-piracy), e.g. using
a foreign `OrderType` and/or for `ValueType`.


## Example of custom order

As an example, let us customize the ordering of floating-point values so that `NaN` are
considered as the largest possible values:

```julia
struct FloatMin <: Base.Order.Ordering end
Base.Order.lt(o::FloatMin, x::T, y::T) where {T<:AbstractFloat} =
    (! isnan(x)) & (isnan(y) | (x < y))
```

The result of `Base.Order.lt(FloatMin(), x, y)` is the same as `isless(x, y)` but the above
implementation of `Base.Order.lt` is faster than `isless` because it uses non-branching
bitwise operators instead of logical ones (then, parentheses are needed owing to the
precedence rules of bitwise operators). This is an example of the benefits of customizing
the ordering of values. Since this amounts to create a special sub-type of
`Base.Order.Ordering`, the same optimization is applicable to other sorting algorithms in
Julia. Incidentally, the above code reflects how [`TotalMin`](@ref) is implemented in the
`QuickHeaps` package (except that  [`TotalMin`](@ref) also account for `missing` values).
