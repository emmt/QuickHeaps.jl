# Customize ordering

If the ordering rules provided by `QuickHeaps` are not suitable for your needs, you may
customize how to order the values in binary heaps or in priority queues. For maximal
flexibility, you may also customize how nodes (not values) are ordered in priority queues.
This is possible because sorting of values and nodes in `QuickHeaps` calls
`QuickHeaps.lt(o::OrderType, x::T, y::T)` to decide whether `x` is strictly before `y`
according to order `o`. For priority queues, `x` and `y` are the queued nodes each combining
a key and a value and the default behavior is to compare the node values. For values (in
binary heaps or in priority queues), the default is that `QuickHeaps.lt` calls
`Base.Order.lt`. These rules are implemented by the following few lines in `QuickHeaps`:

```julia
QuickHeaps.lt(o::Base.Order.Ordering, x::T, y::T) where {T<:QuickHeaps.AbstractNode} =
    QuickHeaps.lt(o, QuickHeaps.get_key(x), QuickHeaps.get_val(y))

QuickHeaps.lt(o::Base.Order.Ordering, x::T, y::T) where {T} =
    Base.Order.lt(o, x, y)
```

In the methods implemented by the `QuickHeaps` package, calls to `Base.Order.lt` and
`QuickHeaps.lt` are always for elements of the same type. Hence the above restriction that
`x` and `y` are both of the same type `T`. This is something to remember when customizing
the behavior of `QuickHeaps.lt`.

## Customize ordering of values

For binary heaps and priority queues with values of type `ValType`, you may create your own
ordering type, say `MyOrder`, inheriting from `Base.Order.Ordering` and specialize
`Base.Order.lt(o::MyOrder, x::ValType, y::ValType)` to yield whether `x` is strictly before
`y` according to `o` and for *values* `x` and `y`. Typically:

```julia
struct MyOrder <: Base.Order.Ordering end
Base.Order.lt(o::MyOrder, x::ValType, y::ValType) = ... # true or false
```

If you do not want to specialize `Base.Order.lt`, you may specialize `QuickHeaps.lt`
instead.

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
the ordering of values. Since this amounts to specializing `Base.Order.lt`, the same
optimization is applicable to other sorting algorithms in Julia. Incidentally, the above
code reflects how [`TotalMin`](@ref) is implemented in the `QuickHeaps` package.

## Customize ordering of nodes

As an additional flexibility, for priority queues with nodes of type `NodeType` and ordering
of type `OrderType`, you may also specialize `QuickHeaps.lt(o::OrderType, x::NodeType,
y::NodeType)` to order the nodes with a specific rule that may account for the keys of the
nodes (not just their values). However to avoid
[type-piracy](https://docs.julialang.org/en/v1/manual/style-guide/#Avoid-type-piracy), at
least one of `NodeType` or `OrderType` must be foreign to `QuickHeaps`.

If you opt on customizing on the ordering type and do not use a custom node type, you must
write something like:

```julia
struct MyOrder <: Base.Order.Ordering end # custom ordering type
function QuickHeaps.lt(o::MyOrder, x::T, y::T) where {T<:QuickHeaps.Node}
    # Below, `QuickHeaps.get_key` and/or `QuickHeaps.get_val` may be called to retrieve
    # the key or value of nodes `x` and `y`.
    true_or_false = ...
    return true_or_false
end
```

The restriction `T <: QuickHeaps.Node` above is because, by default in `QuickHeaps`,
priority queues store their nodes as instances of `QuickHeaps.Node{K,V}`. To allow for a
wider usage, you may replace this by `T <: QuickHeaps.AbstractNode`. As noted in the above
comment [`QuickHeaps.get_key`](@ref) and/or [`QuickHeaps.get_val`](@ref) may be called to
retrieve the key or value of nodes `x` and `y`.

If you opt for using your own node type, you may or not use an existing ordering type but
you must implement a new node type following the guidelines in the [*Node types* section of
the manual](@ref Nodes-types).
