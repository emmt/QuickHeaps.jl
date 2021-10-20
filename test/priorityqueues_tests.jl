module TestingQuicjPriorityQueues

using Test

using Random

using Base: @propagate_inbounds, lt,
    IteratorEltype, HasEltype,
    IteratorSize, HasLength

using QuickHeaps
using QuickHeaps:
    AbstractPriorityQueue, PriorityQueue, FastPriorityQueue,
    AbstractNode, getkey, getval,
    isheap, index, nodes, ordering, in_range, heap_index,
    unsafe_shrink!, unsafe_delete_key!, unsafe_heapify_down!, unsafe_heapify_up!

import Base.Order.Reverse

@testset "Priority queues       " begin

    # Basic checks for an empty priority queue.
    K, V = Int, Float64
    A = PriorityQueue{K,V}()
    @test isempty(A)
    @test length(A) == 0

    @test keytype(A) == K
    @test keytype(typeof(A)) == K
    @test IteratorSize(keys(A)) == HasLength()
    @test IteratorSize(typeof(keys(A))) == HasLength()
    @test IteratorEltype(keys(A)) == HasEltype()
    @test IteratorEltype(typeof(keys(A))) == HasEltype()
    @test eltype(keys(A)) == K
    @test eltype(typeof(keys(A))) == K

    @test valtype(A) == V
    @test valtype(typeof(A)) == V
    @test IteratorSize(values(A)) == HasLength()
    @test IteratorSize(typeof(values(A))) == HasLength()
    @test IteratorEltype(values(A)) == HasEltype()
    @test IteratorEltype(typeof(values(A))) == HasEltype()
    @test eltype(values(A)) == V
    @test eltype(typeof(values(A))) == V

    # Check that `enqueue!` maintains the binary-heap structure.
    n = 1_000
    R = Dict{K,V}() # reference dictionary
    test_1 = true
    for k in 1:n
        v = rand(V)
        enqueue!(A, k, v)
        test_1 &= isheap(nodes(A))
        R[k] = v
    end
    @test test_1
    @test length(A) == n

    # Check `getindex`.
    test_2 = true
    for k in randperm(n)
        test_2 &= (A[k] == R[k])
    end
    @test test_2

    # Check `setindex`.
    B = PriorityQueue{K,V}()
    test_3 = true
    test_4 = true
    for k in keys(A)
        v = A[k]
        B[k] = v
        test_3 &= isheap(nodes(B))
        test_4 &= (haskey(B, k) && B[k] == R[k])
    end
    @test test_3
    @test test_4
    @test length(B) == length(A)
    test_5 = true
    for k in randperm(n)
        test_5 &= (haskey(B, k) && B[k] == R[k])
    end

    # Check result of addressing a non-existing key.
    k = 0
    @test !haskey(A, k)
    @test get(A, k, nothing) === nothing
    @test_throws ArgumentError A[k]

    # Check `first`, `peek`, `keys`, and `values`.
    k, v = first(A)
    @test v == minimum(values(A))
    @test v == first(values(A))
    @test k == first(keys(A))
    x = peek(AbstractNode, A)
    @test v == getval(x)
    @test k == getkey(x)

    # Check that `delete!` maintains the binary-heap structure.
    B = copy(A)
    test_11 = true
    test_12 = true
    test_13 = true
    for i in randperm(n)
        test_11 &= haskey(B, i)
        delete!(B, i)
        test_12 &= !haskey(B, i)
        test_13 &= isheap(nodes(B))
    end
    @test test_11
    @test test_12
    @test test_13
    @test isempty(B)

    # Check that `pop!` extracts nodes in order and maintains the binary-heap
    # structure.
    B = copy(A)
    test_21 = true
    test_22 = true
    prev = typemin(V)
    for i in 1:n
        k, v = pop!(B)
        test_21 &= isheap(nodes(B))
        test_22 &= !(prev > v)
        prev = v
    end
    @test test_21
    @test test_22
    @test isempty(B)

    # Check that unordered `iterate` extracts all nodes without perturbating
    # the original priority queue.
    B = copy(A)
    D = Dict{K,V}()
    test_31 = true
    for (k,v) in A
        test_31 &= !haskey(D, k)
        D[k] = v
    end
    @test test_31
    @test length(D) == n
    @test nodes(A) == nodes(B)
    @test index(A) == index(B)
    test_32 = true
    for (k,v) in D
        test_32 &= haskey(A, k)
    end
    @test test_32

    # Test `keys` and `values` yield all elements in the same order as `iterate`.
    test_33 = true
    test_34 = true
    for (k,v,kv) in zip(keys(A), values(A), A)
        test_33 &= (k == kv.first)
        test_34 &= (v == kv.second)
    end
    @test test_33
    @test test_34

end # @testset

end # module
