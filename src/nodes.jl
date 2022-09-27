"""
    QuickHeaps.AbstractNode{K,V}

is the super-type of nodes with a key of type `K` and a value of type `V`.
Nodes can be used in binary heaps and priority queues to represent
key-value pairs and specific ordering rules may be imposed by specializing the
`Base.lt` method which is by default:

    Base.lt(o::Ordering, a::T, b::T) where {T<:QuickHeaps.AbstractNode} =
        lt(o, QuickHeaps.getval(a), QuickHeaps.getval(b))

"""
abstract type AbstractNode{K,V} end

"""
    QuickHeaps.Node{K=typeof(k),V=typeof(v)}(k,v)

yields a node storing key `k` and value `v`. Optional type parameters `K` and
`V` are the respective types of the key and of the value.

See also [`QuickHeaps.AbstractNode`](@ref),
[`QuickHeaps.AbstractPriorityQueue`](@ref).

"""
struct Node{K,V} <: AbstractNode{K,V}
    key::K
    val::V
    Node{K,V}(key, val) where {K,V} = new{K,V}(key, val)
end
Node{K}(key, val::V) where {K,V} = Node{K,V}(key, val)
Node(key::K, val::V) where {K,V} = Node{K,V}(key, val)

"""
    getkey(x::QuickHeaps.AbstractNode) -> k

yields the key `k` of node `x`. This method may be specialized for any
sub-types of [`QuickHeaps.AbstractNode`](@ref).

Also see [`QuickHeaps.getval`](@ref).

"""
getkey(x::Node) = getfield(x, :key)

"""
    QuickHeaps.getval(x::QuickHeaps.AbstractNode) -> v

yields the value `v` of node `x`. This method may be specialized for any
sub-types of [`QuickHeaps.AbstractNode`](@ref).

Also see [`getkey(::QuickHeaps.AbstractNode)`](@ref).

"""
getval(x::Node) = getfield(x, :val)

for type in (:AbstractNode, :Node)
    @eval begin
        $type(x::$type) = x
        $type{K}(x::$type{K}) where {K} = x
        $type{K,V}(x::$type{K,V}) where {K,V} = x
    end
end
Node(x::AbstractNode) = Node(getkey(x), getval(x))
Node{K}(x::AbstractNode) where {K} = Node{K}(getkey(x), getval(x))
Node{K,V}(x::AbstractNode) where {K,V} = Node{K,V}(getkey(x), getval(x))

Node(x::Tuple{Any,Any}) = Node(x[1], x[2])
Node{K}(x::Tuple{Any,Any}) where {K} = Node{K}(x[1], x[2])
Node{K,V}(x::Tuple{Any,Any}) where {K,V} = Node{K,V}(x[1], x[2])
Tuple(x::AbstractNode) = (getkey(x), getval(x))

Node(x::Pair) = Node(x.first, x.second)
Node{K}(x::Pair) where {K} = Node{K}(x.first, x.second)
Node{K,V}(x::Pair) where {K,V} = Node{K,V}(x.first, x.second)
Pair(x::AbstractNode) = getkey(x) => getval(x)

Base.convert(::Type{T}, x::T) where {T<:AbstractNode} = x
Base.convert(::Type{T}, x::AbstractNode) where {T<:AbstractNode} = T(x)
Base.convert(::Type{T}, x::Tuple{Any,Any}) where {T<:AbstractNode} = T(x)
Base.convert(::Type{T}, x::Pair) where {T<:AbstractNode} = T(x)

iterate(x::AbstractNode) = (getkey(x), first)
iterate(x::AbstractNode, ::typeof(first)) = (getval(x), last)
iterate(x::AbstractNode, ::typeof(last)) = nothing

# Nodes are sorted according to their values.
for O in (:Ordering, :ForwardOrdering, :ReverseOrdering, :FastForwardOrdering)
    @eval begin
        lt(o::$O, a::T, b::T) where {T<:AbstractNode} =
            lt(o, getval(a), getval(b))
    end
end
