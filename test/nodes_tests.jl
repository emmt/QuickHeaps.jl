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
        x = Node(k, v)
        @test isa(x, AbstractNode)
        @test isa(x, Node)
        @test isa(x, AbstractNode{K,V})
        @test isa(x, Node{K,V})
        # Node <-> Tuple
        @test Tuple(x) === (k, v)
        @test Node((k, v)) === x
        @test convert(Node, (k, v)) === x
        @test convert(Node{K}, (k, v)) === x
        @test convert(Node{K,V}, (k, v)) === x
        kp, vp = x
        @test kp === k && vp === v
        # Node <-> Pair
        @test Pair(x) === (k => v)
        @test Node(k => v) === x
        @test convert(Node, k => v) === x
        @test convert(Node{K}, k => v) === x
        @test convert(Node{K,V}, k => v) === x
        # Node <-> (Abstract)Node
        @test AbstractNode(x) === x
        @test AbstractNode{K}(x) === x
        @test AbstractNode{K,V}(x) === x
        @test convert(AbstractNode, x) === x
        @test convert(AbstractNode{K}, x) === x
        @test convert(AbstractNode{K,V}, x) === x
        @test Node(x) === x
        @test Node{K}(x) === x
        @test Node{K,V}(x) === x
        @test convert(Node, x) === x
        @test convert(Node{K}, x) === x
        @test convert(Node{K,V}, x) === x
        # Iterator.
        r1 = iterate(x)
        @test r1 isa Tuple{Any,Any} && r1[1] === getkey(x)
        if r1 isa Tuple{Any,Any}
            r2 = iterate(x, r1[2])
            @test r2 isa Tuple{Any,Any} && r2[1] === getval(x)
            if r2 isa Tuple{Any,Any}
                @test iterate(x, r2[2]) isa Nothing
            end
        end
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
        x = KeyOnlyNode(k)
        @test isa(x, AbstractNode)
        @test isa(x, KeyOnlyNode)
        @test isa(x, AbstractNode{K,Nothing})
        @test isa(x, KeyOnlyNode{K})
        @test Tuple(x) === (k, nothing)
        @test KeyOnlyNode((k, nothing)) === x
        @test Pair(x) === (k => nothing)
        @test KeyOnlyNode(k => nothing) === x
        kp, vp = x
        V = typeof(vp)
        @test kp === k && vp === nothing
        @test Node(x) === Node(kp, vp)
        @test Node{K}(x) === Node(kp, vp)
        @test Node{K,V}(x) === Node(kp, vp)
        @test convert(Node, x) === Node(kp, vp)
        @test convert(Node{K}, x) === Node(kp, vp)
        @test convert(Node{K,V}, x) === Node(kp, vp)
    end
end

end # module
