module TestingQuicjPriorityQueues

using Test

using Random

using Base: @propagate_inbounds, lt,
    ReverseOrdering, Reverse,
    IteratorEltype, HasEltype,
    IteratorSize, HasLength

using QuickHeaps
using QuickHeaps:
    AbstractPriorityQueue, PriorityQueue, FastPriorityQueue,
    AbstractNode, getkey, getval,
    isheap, index, nodes, ordering, in_range, heap_index

function test_queue!(A::AbstractPriorityQueue{K,V},
                     key_list::AbstractVector{K},
                     val_list::AbstractVector{V}) where {K,V}
    # Check arguments.
    axes(val_list) == axes(key_list) || error("incompatible indices")
    n = length(key_list)
    axes(key_list) == (1:n,) || error("non-standard indices")
    o = ordering(A)

    # Dummy private function used to "label" the tests.
    check(flag::Bool, comment) = flag

    # Checks keytype, etc.
    @test keytype(A) == K
    @test keytype(typeof(A)) == K
    @test IteratorSize(keys(A)) == HasLength()
    @test IteratorSize(typeof(keys(A))) == HasLength()
    @test IteratorEltype(keys(A)) == HasEltype()
    @test IteratorEltype(typeof(keys(A))) == HasEltype()
    @test eltype(keys(A)) == K
    @test eltype(typeof(keys(A))) == K

    # Checks valtype, etc.
    @test valtype(A) == V
    @test valtype(typeof(A)) == V
    @test IteratorSize(values(A)) == HasLength()
    @test IteratorSize(typeof(values(A))) == HasLength()
    @test IteratorEltype(values(A)) == HasEltype()
    @test IteratorEltype(typeof(values(A))) == HasEltype()
    @test eltype(values(A)) == V
    @test eltype(typeof(values(A))) == V

    # Check that `enqueue!` maintains the binary-heap structure.
    R = Dict{K,V}() # reference dictionary
    test_1 = true
    test_2 = true
    for (k, v) in zip(key_list, val_list)
        enqueue!(A, k, v)
        test_1 &= isheap(o, nodes(A))
        test_2 &= !haskey(R, k) # unique key?
        R[k] = v
    end
    @test check(test_1, "binary-heap structure after `enqueue!`")
    @test check(test_2, "unique keys after `enqueue!`")
    @test length(A) == n

    # Check `first`, `peek`, `keys`, and `values`.
    k, v = first(A)
    @test v == (isa(o, ReverseOrdering) ? maximum : minimum)(values(A))
    @test v == first(values(A))
    @test k == first(keys(A))
    x = peek(AbstractNode, A)
    @test v == getval(x)
    @test k == getkey(x)

    # Check `getindex` in random order.
    test_1 = true
    for i in randperm(n)
        k = key_list[i]
        test_1 &= (haskey(A,k) && A[k] == R[k])
    end
    @test check(test_1, "`getindex` in random order")

    # Test that `keys` and `values` yield all elements in the same order as
    # `iterate`.
    test_1 = true
    test_2 = true
    for (k,v,kv) in zip(keys(A), values(A), A)
        test_1 &= (k == kv.first)
        test_2 &= (v == kv.second)
    end
    @test check(test_1, "keys in heap order")
    @test check(test_2, "values in heap order")

    # Check `copy`.
    B = copy(A)
    @test typeof(B) === typeof(A)
    @test length(B) == length(A)
    @test nodes(A) == nodes(B)
    @test index(A) == index(B)
    S = Set(keys(B))
    @test length(S) == length(B) # keys are unique?
    test_1 = true
    for k in S
        test_1 &= (haskey(A, k) && haskey(B, k) && A[k] == B[k])
    end
    @test check(test_1, "`copy` yields same nodes")

    # Check `delete!` and result of addressing a non-existing key.
    k = key_list[rand(1:n)]
    delete!(B, k)
    @test !haskey(B, k)
    @test get(B, k, nothing) === nothing
    @test_throws ArgumentError B[k]

    # Check `isempty`, `empty!`, etc.
    empty!(B)
    @test isempty(B)
    @test length(B) == 0
    @test length(A) == n # no side effects on A

    # Check `setindex!` in heap order.
    length(B) > 1 && empty!(B)
    test_1 = true
    test_2 = true
    for i in randperm(n)
        k, v = key_list[i], val_list[i]
        B[k] = v
        test_1 &= isheap(o, nodes(B))
        test_2 &= (haskey(B, k) && B[k] == R[k])
    end
    @test check(test_1, "heap structure preserved by `setindex!`")
    @test check(test_2, "same value for key after `setindex!`")
    @test length(B) == length(A)
    test_1 = true
    test_2 = true
    for i in randperm(n)
        k = key_list[i]
        test_1 &= haskey(B, k)
        test_2 &= B[k] == R[k]
    end
    @test check(test_1, "no missing keys in random order")
    @test check(test_2, "same values in random order")

    # Check that `delete!` maintains the binary-heap structure.
    test_1 = true
    test_2 = true
    test_3 = true
    for i in randperm(n)
        k = key_list[i]
        test_1 &= haskey(B, k)
        delete!(B, k)
        test_2 &= isheap(o, nodes(B))
        test_3 &= !haskey(B, k)
    end
    @test check(test_1, "keys exist before `delete!`")
    @test check(test_2, "heap structure preserved by `delete!`")
    @test check(test_3, "keys do not exist after `delete!`")
    @test isempty(B)

    # Check that `pop!` extracts nodes in order and maintains the binary-heap
    # structure.
    B = copy(A)
    test_1 = true
    test_2 = true
    prev = (isa(o, ReverseOrdering) ? typemax : typemin)(V)
    for i in 1:n
        k, v = pop!(B)
        test_1 &= isheap(o, nodes(B))
        test_2 &= !lt(o, v, prev)
        prev = v
    end
    @test check(test_1, "heap structure preserved by `pop!`")
    @test check(test_2, "`pop!` yields keys in order")
    @test isempty(B)

    # Tests for fast priority queues.
    if isa(A, FastPriorityQueue)
        cartesian_index = CartesianIndices(index(A))
        linear_index = LinearIndices(index(A))
        test_1 = true
        test_2 = true
        test_3 = true
        test_4 = true
        test_5 = true
        test_6 = true
        test_7 = true
        test_8 = true
        for i in randperm(n)
            k = key_list[i]
            c = cartesian_index[k]
            inds = c.I
            test_1 &= haskey(A, c)
            test_2 &= haskey(A, inds)
            test_3 &= (A[k] == A[c])
            test_4 &= (A[k] == A[inds...])
            v = A[k]
            test_5 &= !haskey(delete!(A, c), k)
            A[c] = v
            test_6 &= (haskey(A, k) && A[k] == v)
            test_7 &= !haskey(delete!(A, inds), k)
            A[inds...] = v
            test_8 &= (haskey(A, k) && A[k] == v)
        end
        @test check(test_1, "`haskey` with Cartesian index")
        @test check(test_2, "`haskey` with multi-dimensional index")
        @test check(test_3, "`getindex` with Cartesian index")
        @test check(test_4, "`getindex` with multi-dimensional index")
        @test check(test_5, "`delete!` with Cartesian index")
        @test check(test_6, "`setindex!` with Cartesian index")
        @test check(test_7, "`delete!` with multi-dimensional index")
        @test check(test_8, "`setindex!` with multi-dimensional index")
    end
end

@testset "Priority queues       " begin
    K, V, n = Int, Float64, 20
    key_list = map(K, 1:n)
    val_list = rand(V, n)
    test_queue!(PriorityQueue{K,V}(), key_list, val_list)
    test_queue!(PriorityQueue{K,V}(Reverse), key_list, val_list)
end

@testset "Fast priority queues  " begin
    V, dims = Float32, (2,3,4)
    n = prod(dims)
    m = div(9n + 5, 10) # keep ~90% of indices
    key_list = randperm(n)[1:m]
    val_list = rand(V, m)
    test_queue!(FastPriorityQueue{V}(dims), key_list, val_list)
    test_queue!(FastPriorityQueue{V}(Reverse, dims), key_list, val_list)
end

end # module
