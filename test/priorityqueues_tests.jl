module TestingQuicjPriorityQueues

using Test

using Random

using Base: @propagate_inbounds, lt

using QuickHeaps
using QuickHeaps:
    AbstractPriorityQueue, PriorityQueue, FastPriorityQueue,
    _PriorityQueueIterator,
    isheap, index, nodes, ordering, in_range, heap_index,
    unsafe_shrink!, unsafe_delete_key!, unsafe_heapify_down!, unsafe_heapify_up!

import Base.Order.Reverse

@testset "Priority queues       " begin

    # Check that `enqueue!` maintains the binary-heap structure.
    T, n = Float64, 10_000
    A = PriorityQueue{Int,T}()
    @test keytype(A) == Int
    @test valtype(A) == T
    test_1 = true
    for i in 1:n
        enqueue!(A, i, rand(T))
        test_1 &= isheap(nodes(A))
    end
    @test test_1
    @test length(A) == n

    # Check that `delete!` maintains the binary-heap structure.
    B = copy(A)
    test_2 = true
    test_3 = true
    test_4 = true
    for i in randperm(n)
        test_2 &= haskey(B, i)
        delete!(B, i)
        test_3 &= !haskey(B, i)
        test_4 &= isheap(nodes(B))
    end
    @test test_2
    @test test_3
    @test test_4
    @test isempty(B)

    # Check that `pop!` extracts nodes in order and maintains the binary-heap
    # structure.
    B = copy(A)
    test_5 = true
    test_6 = true
    prev = typemin(T)
    for i in 1:n
        k, v = pop!(B)
        test_5 &= isheap(nodes(B))
        test_6 &= !(prev > v)
        prev = v
    end
    @test test_5
    @test test_6
    @test isempty(B)

    # Check that unordered `iterate` extracts all nodes without perturbating
    # the original priority queue.
    B = copy(A)
    K, V = keytype(A), valtype(A)
    D = Dict{K,V}()
    test_7 = true
    for (k,v) in A
        test_7 &= !haskey(D, k)
        D[k] = v
    end
    @test test_7
    @test length(D) == n
    @test nodes(A) == nodes(B)
    @test index(A) == index(B)
    test_8 = true
    for (k,v) in D
        test_8 &= haskey(A, k)
    end
    @test test_8

    # Test `keys` and `values` yield all elements in the same order as `iterate`.
    test_10 = true
    test_11 = true
    for (k,v,kv) in zip(keys(A), values(A), A)
        test_10 &= (k == kv.first)
        test_11 &= (v == kv.second)
    end
    @test test_10
    @test test_11

end # @testset

end # module
