# Nodes types

Nodes in priority queues provided by `QuickHeaps` have super-type:

```julia
QuickHeaps.AbstractNode{K,V}
```

with `K` and `V` the respective types of the key and of the value of the node. In principle,
priority of a node is based on its value, but this may be changed by using custom node
and/or ordering types.

Having a specific node type different than, say, `Pair{K,V}` is to allow customizing how the
nodes are compared for ordering by specializing `Base.lt` without
[type-piracy](https://docs.julialang.org/en/v1/manual/style-guide/#Avoid-type-piracy).

A node `x` can be iterated and converted into a pair or a 2-tuple of its key `k` and its
value `v` and conversely:

```julia
k, v = x                # extract key and value of a node
Pair(x)                 # yields k=>v
Tuple(x)                # yields (k,v)
QuickHeaps.Node((k, v)) # is the same as QuickHeaps.Node(k, v)
QuickHeaps.Node(k => v) # is the same as QuickHeaps.Node(k, v)
```

The `getkey` and (unexported) [`QuickHeaps.getval`](@ref) methods respectively retrieve the
key and the value of a node. These two methods may be specialized for a given sub-type of
[`QuickHeaps.AbstractNode`](@ref). For example, a *key-only* node type can be fully
implemented by:

```julia
struct KeyOnlyNode{K} <: QuickHeaps.AbstractNode{K,Nothing}
    key::K
end
QuickHeaps.getkey(x::KeyOnlyNode) = getfield(x, :key)
QuickHeaps.getval(x::KeyOnlyNode) = nothing
KeyOnlyNode(x::Tuple{K,Nothing}) where {K} = KeyOnlyNode{K}(x[1])
KeyOnlyNode(x::Pair{K,Nothing}) where {K} = KeyOnlyNode{K}(x.first)
```

To provide your own ordering rules, you may specialize `Base.lt` which otherwise defaults
to:

```julia
Base.lt(o::Ordering, a::QuickHeaps.AbstractNode, b::QuickHeaps.AbstractNode) =
    lt(o, QuickHeaps.getval(a), QuickHeaps.getval(b))
```
