module TestingQuickPriorityQueues

using Test

using Random

using Base: @propagate_inbounds,
    IteratorEltype, HasEltype,
    IteratorSize, HasLength
using Base.Order: Ordering, Forward, ReverseOrdering, Reverse, lt

using QuickHeaps

pass(::Test.Pass) = true
pass(::Test.Result) = false

function test_queue!(A::AbstractPriorityQueue{K,V},
                     key_list::AbstractVector{K},
                     val_list::AbstractVector{V}) where {K,V}
    # Check arguments.
    axes(val_list) == axes(key_list) || error("incompatible indices")
    n = length(key_list)
    axes(key_list) == (1:n,) || error("non-standard indices")
    o = Ordering(A)

    # Checks keytype, etc.
    @test keytype(A) == K
    @test keytype(typeof(A)) == K
    @test IteratorSize(keys(A)) == HasLength()
    @test IteratorSize(typeof(keys(A))) == HasLength()
    @test length(keys(A)) == length(A)
    @test IteratorEltype(keys(A)) == HasEltype()
    @test IteratorEltype(typeof(keys(A))) == HasEltype()
    @test eltype(keys(A)) == K
    @test eltype(typeof(keys(A))) == K

    # Checks valtype, etc.
    @test valtype(A) == V
    @test valtype(typeof(A)) == V
    @test IteratorSize(values(A)) == HasLength()
    @test IteratorSize(typeof(values(A))) == HasLength()
    @test length(values(A)) == length(A)
    @test IteratorEltype(values(A)) == HasEltype()
    @test IteratorEltype(typeof(values(A))) == HasEltype()
    @test eltype(values(A)) == V
    @test eltype(typeof(values(A))) == V

    # Check that `enqueue!`, `push!`, and `setindex!` maintain the binary-heap
    # structure.
    @test isempty(A)
    R = Dict{K,V}() # reference dictionary
    for (k, v) in zip(key_list, val_list)
        # Track pushed pairs.
        R[k] = v
        n = length(R) # to alternate between the equivalent methods
        if (n % 4) == 1
            pass(@test (@inferred enqueue!(A, k, v)) === A) || break
        elseif (n % 4) == 2
            pass(@test (@inferred enqueue!(A, k => v)) === A) || break
        elseif (n % 4) == 3
            pass(@test (@inferred push!(A, k => v)) === A) || break
        else
            pass(@test (@inferred setindex!(A, v, k)) === A) || break
        end
        pass(@test length(A) == length(R)) || break

        # Check that the key=>val content is correct and that `iterate`, `values` and `keys`
        # iterate correctly and keep the same order.
        ps = @inferred collect(A)
        pass(@test ps isa Vector{Pair{K,V}}) || break
        pass(@test length(ps) == length(A)) || break

        ks = @inferred collect(keys(A))
        pass(@test ks isa Vector{K}) || break
        pass(@test length(ks) == length(A)) || break
        pass(@test ks == [k for (k,v) in ps]) || break
        pass(@test sort(ks) == sort(collect(keys(R)))) || break

        vs = @inferred collect(values(A))
        pass(@test vs isa Vector{V}) || break
        pass(@test length(vs) == length(A)) || break
        pass(@test vs == [v for (k,v) in ps]) || break
        pass(@test sort(vs) == sort(collect(values(R)))) || break

        # Values must form a heap.
        pass(@test isheap(vs, o)) || break
    end

    # Check `first`, `peek`, `keys`, and `values`.
    k, v = first(A)
    @test v == (isa(o, ReverseOrdering) ? maximum : minimum)(values(A))
    @test v == first(values(A))
    @test k == first(keys(A))
    x = @inferred peek(A)
    @test x isa Pair
    @test x == (k => v)

    # Check `getindex`, `get`, and `getkey` in random order.
    for i in randperm(length(key_list))
        k = key_list[i]
        pass(@test haskey(A, k)) || break
        pass(@test A[k] == R[k]) || break
        pass(@test get(A, k, undef) == R[k]) || break
        pass(@test getkey(A, k, undef) == k) || break
    end

    # Check `copy`.
    B = @inferred copy(A)
    @test typeof(B) === typeof(A)
    @test length(B) == length(A)
    @test collect(A) == collect(B) # same pairs
    @test collect(keys(A)) == collect(keys(B)) # same keys
    @test collect(values(A)) == collect(values(B)) # same values
    @test A.index == B.index # same heap to index

    # Check `delete!` and result of addressing a non-existing key.
    for key in (undef, Int)
        @test !haskey(B, key)
        @test delete!(B, key) === B
        @test get(B, key, missing) === missing
        @test_throws KeyError B[key]
    end

    # Check `isempty`, `empty!`, etc.
    @test (@inferred empty!(B)) === B
    @test isempty(B)
    @test length(B) == 0
    @test length(A) == length(R) # no side effects on A

    # Check `setindex!` in heap order.
    length(B) > 1 && empty!(B)
    for i in randperm(length(key_list))
        k, v = key_list[i], val_list[i]
        B[k] = v
        pass(@test haskey(B, k)) || break
        pass(@test B[k] == R[k]) || break
        pass(@test isheap(collect(values(B)), o)) || break
    end
    @test length(B) == length(A)
    for i in randperm(length(key_list))
        k = key_list[i]
        pass(@test haskey(B, k)) || break
        pass(@test B[k] == R[k]) || break
    end

    # Check that `delete!` maintains the binary-heap structure.
    for i in randperm(length(key_list))
        k = key_list[i]
        pass(@test haskey(B, k)) || break
        pass(@test delete!(B, k) === B) || break
        pass(@test !haskey(B, k)) || break
        pass(@test isheap(collect(values(B)), o)) || break
    end
    @test isempty(B)

    # Check that `pop!` and `dequeue_pair!` extracts nodes in order and maintains the
    # binary-heap structure.
    B = @inferred copy(A)
    prev = (isa(o, ReverseOrdering) ? typemax : typemin)(V)
    for i in 1:length(B)
        local x
        if isodd(i % 2) == 1
            x = @inferred pop!(B)
            pass(@test x isa Pair) || break
        else
            x = @inferred dequeue_pair!(B)
            pass(@test x isa Pair) || break
        end
        k, v = x
        pass(@test isheap(collect(values(B)), o)) || break
        pass(@test !lt(o, v, prev)) || break
        prev = v
    end
    @test isempty(B)

    # Tests for fast priority queues.
    if isa(A, FastPriorityQueue)
        empty!(A)
        for (k,v) in zip(key_list, val_list)
            A[k] = v
        end
        cartesian_index = CartesianIndices(A.index)
        linear_index = LinearIndices(A.index)
        N = ndims(cartesian_index)
        for i in randperm(length(key_list))
            k = key_list[i]
            v = val_list[i]
            c = cartesian_index[k]
            inds = Tuple(c)
            inds1 = (CartesianIndex(),
                     inds[1:end-1]...,
                     CartesianIndex(),
                     CartesianIndex(inds[end]))
            inds2 = map(CartesianIndex, inds)
            pass(@test haskey(A, c)) || break
            pass(@test A[c] == v) || break
            pass(@test A[inds...] == v) || break
            pass(@test A[inds1...] == v) || break
            pass(@test A[inds2...] == v) || break
            pass(@test get(A, k, undef) == v) || break
            pass(@test get(A, c, undef) == v) || break
            pass(@test getkey(A, k, undef) == k) || break
            pass(@test getkey(A, c, undef) == k) || break
            pass(@test delete!(A, c) === A) || break
            pass(@test !haskey(A, k)) || break
            pass(@test !haskey(A, c)) || break
            pass(@test get(A, k, undef) === undef) || break
            pass(@test get(A, c, undef) === undef) || break
            pass(@test getkey(A, k, undef) === undef) || break
            pass(@test getkey(A, c, undef) === undef) || break
            A[c] = v
            pass(@test get(A, k, undef) == v) || break
            pass(@test getkey(A, k, undef) == k) || break
            pass(@test !haskey(delete!(A, c), k)) || break
            A[inds...] = v
            pass(@test get(A, k, undef) == v) || break
            pass(@test getkey(A, k, undef) == k) || break
            pass(@test !haskey(delete!(A, c), k)) || break
            A[inds1...] = v
            pass(@test get(A, k, undef) == v) || break
            pass(@test getkey(A, k, undef) == k) || break
            pass(@test !haskey(delete!(A, c), k)) || break
            A[inds2...] = v
            pass(@test get(A, k, undef) == v) || break
            pass(@test getkey(A, k, undef) == k) || break
        end
        i_first = first(linear_index)
        i_last = last(linear_index)
        @test !haskey(A, i_first - 1)
        @test !haskey(A, i_last  + 1)
        i_first = first(cartesian_index)
        i_last = last(cartesian_index)
        for d in 1:N
            s = CartesianIndex(ntuple(i -> i == d, N))
            pass(@test !haskey(A, i_first - s)) || break
            pass(@test !haskey(A, i_last  + s)) || break
        end
    end
end

@testset "Indexing utilities    " begin
    let normalize_indices = QuickHeaps.normalize_indices
        @test normalize_indices(-17) === -17
        @test normalize_indices(0x09) === 9
        @test normalize_indices(CartesianIndex(-1)) === (-1,)
        @test normalize_indices(CartesianIndex(-1,2)) === (-1,2)
        @test normalize_indices(CartesianIndex(-1,-2,-3)) === (-1,-2,-3)
        @test normalize_indices((0,0x01,-2,)) === (0,1,-2)
        @test normalize_indices((0,CartesianIndex(1,2),0x03,CartesianIndex())) === (0,1,2,3)
        @test normalize_indices((0x00,CartesianIndex(),-1,CartesianIndex(-2),CartesianIndex(),0x03,CartesianIndex(-4,5))) === (0,-1,-2,3,-4,5)
    end
end

@testset "Priority queues       " begin
    @test QuickHeaps.default_ordering(AbstractPriorityQueue) === TotalMin
    @test QuickHeaps.default_ordering(PriorityQueue) === TotalMin
    K, V, n = Int, Float64, 20
    key_list = map(K, 1:n)
    val_list = rand(V, n)
    test_queue!(PriorityQueue{K,V}(), key_list, val_list)
    test_queue!(PriorityQueue{K,V}(Reverse), key_list, val_list)
end

@testset "Fast priority queues  " begin
    @test QuickHeaps.default_ordering(FastPriorityQueue) === TotalMin
    V, dims = Float32, (2,3,4)
    n = prod(dims)
    m = div(9n + 5, 10) # keep ~90% of indices
    key_list = randperm(n)[1:m]
    val_list = rand(V, m)
    test_queue!(FastPriorityQueue{V}(dims), key_list, val_list)
    test_queue!(FastPriorityQueue{V}(Reverse, dims), key_list, val_list)
end

end # module
