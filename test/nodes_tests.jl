module TestingQuickNodes

using Test

using Base: Ordering, ForwardOrdering, ReverseOrdering, Forward, Reverse, lt

using QuickHeaps
using QuickHeaps:
    AbstractNode, Node
import QuickHeaps: getkey, getval

@testset "Standard nodes        " begin
    for (k, v) in (("foo", 2.1), (:bar, 11), (CartesianIndex(11,2), nothing))
        K = typeof(k)
        V = typeof(v)
        nd = Node(k, v)
        @test isa(nd, AbstractNode)
        @test isa(nd, Node)
        @test isa(nd, AbstractNode{K,V})
        @test isa(nd, Node{K,V})
        @test Tuple(nd) === (k, v)
        @test Node((k, v)) === nd
        @test Pair(nd) === (k => v)
        @test Node(k => v) === nd
        kp, vp = nd
        @test kp === k && vp === v
    end
end

struct KeyOnlyNode{K} <: AbstractNode{K,Nothing}
    key::K
end
getkey(x::KeyOnlyNode) = getfield(x, :key)
getval(x::KeyOnlyNode) = nothing
KeyOnlyNode(x::Tuple{K,Nothing}) where {K} = KeyOnlyNode{K}(x[1])
KeyOnlyNode(x::Pair{K,Nothing}) where {K} = KeyOnlyNode{K}(x.first)

@testset "Custom nodes          " begin
    for k in ("foo", :bar, CartesianIndex(1,2))
        K = typeof(k)
        nd = KeyOnlyNode(k)
        @test isa(nd, AbstractNode)
        @test isa(nd, KeyOnlyNode)
        @test isa(nd, AbstractNode{K,Nothing})
        @test isa(nd, KeyOnlyNode{K})
        @test Tuple(nd) === (k, nothing)
        @test KeyOnlyNode((k, nothing)) === nd
        @test Pair(nd) === (k => nothing)
        @test KeyOnlyNode(k => nothing) === nd
        kp, vp = nd
        @test kp === k && vp === nothing
    end
end

end # module
