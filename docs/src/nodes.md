# Nodes types

Nodes in priority queues provided by `QuickHeaps` have super-type:

```julia
QuickHeaps.AbstractNode{K,V}
```

with `K` and `V` the respective types of the key and of the value of the node. In principle,
priority of a node is based on its value, but this may be changed by using custom node
and/or ordering types.

Having a specific node type different than, say, `Pair{K,V}` is to allow customizing how the
nodes are compared for ordering by specializing `QuickHeaps.lt` without
[type-piracy](https://docs.julialang.org/en/v1/manual/style-guide/#Avoid-type-piracy).

A node `x` can be iterated and converted into a pair or a 2-tuple of its `key` and its value
`val` and conversely:

```julia
x = QuickHeaps.Node(key, val)   # builds a node
x = QuickHeaps.Node((key, val)) # idem
x = QuickHeaps.Node(key => val) # idem
key, val = x                    # extract key and value of a node
Pair(x)                         # yields key=>val
Tuple(x)                        # yields (key,val)
```

The non-exported methods [`QuickHeaps.get_key(x)`](@ref QuickHeaps.get_key) and
[`QuickHeaps.get_val(x)`](@ref QuickHeaps.get_val) methods respectively retrieve the key and
the value of a node. These two methods may be specialized for a given sub-type of
[`QuickHeaps.AbstractNode`](@ref). For example, a *key-only* node type can be fully
implemented by:

```julia
struct KeyOnlyNode{K} <: QuickHeaps.AbstractNode{K,Nothing}
    key::K
end
QuickHeaps.get_key(x::KeyOnlyNode) = getfield(x, :key)
QuickHeaps.get_val(x::KeyOnlyNode) = nothing
KeyOnlyNode(key::K, ::Nothing) where {K} = KeyOnlyNode{K}(key)
KeyOnlyNode((key, _)::Union{Tuple{K,V},Pair{K,V}}) where {K,V<:Nothing} = KeyOnlyNode{K}(key)
```

To provide your own ordering rules, you may specialize [`QuickHeaps.lt`](@ref) which
otherwise defaults to:

```julia
QuickHeaps.lt(o::Ordering, x::QuickHeaps.AbstractNode, y::QuickHeaps.AbstractNode) =
    Base.Order.lt(o, QuickHeaps.get_val(x), QuickHeaps.get_val(y))
```
