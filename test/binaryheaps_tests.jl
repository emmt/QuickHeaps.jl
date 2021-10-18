module TestingBinaryHeaps

using Test

using Base: Ordering, ForwardOrdering, ReverseOrdering, Forward, Reverse, lt

using QuickHeaps
using QuickHeaps:
    AbstractBinaryHeap,
    FastForwardOrdering, FastForward, FastReverse,
    FastMin, FastMax, SafeMin, SafeMax,
    isheap, heapify, heapify!,
    ordering

# FIXME: This should be done by default by the `reverse` method.
reverseordering(o::ForwardOrdering) = ReverseOrdering(o)
reverseordering(o::FastForwardOrdering) = ReverseOrdering(o)
reverseordering(o::ReverseOrdering) = o.fwd

is_max_ordering(x) = !is_min_ordering(x)
is_min_ordering(o::Ordering) = is_min_ordering(typeof(o))
is_min_ordering(::AbstractBinaryHeap{<:Any,O}) where {O} = is_min_ordering(O)
is_min_ordering(::Type{<:FastForwardOrdering}) = true
is_min_ordering(::Type{<:ForwardOrdering}) = true
is_min_ordering(::Type{<:ReverseOrdering{O}}) where {O} = is_max_ordering(O)

function is_sorted(o::Base.Ordering, x::AbstractVector)
    flag = false
    for i in 2:length(x)
        flag |= lt(o, x[i], x[i-1])
    end
    return !flag
end

function heap_test(T::Type, h::AbstractBinaryHeap, n::Integer = 7)
    # Build array of values.  For n ≥ 3, it should not be a heap in any order.
    n ≥ 3 || error("number of elements must be at least 3")
    A = T[(2:2:n)..., (1:2:n)...]
    @test !isheap(A, Forward)
    @test !isheap(A, Reverse)

    @test eltype(h) === T
    @test size(h) == (length(h),)
    @test IndexStyle(h) == IndexLinear()
    empty!(h)
    @test isempty(h)
    @test isheap(h; check=true)
    @test_throws ArgumentError peek(h)
    @test_throws ArgumentError pop!(h)
    for i in 1:n
        push!(h, rand(T))
        @test isheap(h; check=true)
        @test !isempty(h)
        @test length(h) == i
        v1 = (is_min_ordering(h) ? minimum(h) : maximum(h))
        @test peek(h) == v1
        @test first(h) == v1
    end
    for i in 1:n
        h[i] = rand(T)
        @test isheap(h; check=true)
        @test length(h) == n
        v1 = (is_min_ordering(h) ? minimum(h) : maximum(h))
        @test peek(h) == v1
        @test first(h) == v1
    end
    x = T[]
    while !isempty(h)
        push!(x, pop!(h))
    end
    @test length(x) == n
    @test is_sorted(ordering(h), x)
end

@testset "Arrays as binary heaps" begin
    # Min-heap.
    let A = [1, 4, 2, 6, 5, 3, 8, 7, 11, 13, 10, 14, 9, 12]
        n = length(A)
        @test isheap(A)
        @test isheap(A, n)
        @test isheap(A, n - 1) # last element can be discarded
        for o in (Forward, FastForward)
            @test isheap(A, o)
            @test isheap(o, A)
            @test isheap(A, n, o)
            @test isheap(A, o, Int16(n))
            @test isheap(o, A, n)
            @test isheap(o, A, n - 1) # last element can be discarded
            @test !isheap(A, reverseordering(o))
        end
    end
    # Max-heap.
    let A = [14, 11, 13, 9, 10, 12, 7, 8, 4, 2, 5, 1, 6, 3]
        n = length(A)
        @test !isheap(A)
        for o in (Reverse, FastReverse)
            @test isheap(A, o)
            @test isheap(o, A)
            @test isheap(A, n, o)
            @test isheap(A, o, n)
            @test isheap(o, A, n)
            @test isheap(o, A, n - 1) # last element can be discarded
            @test !isheap(A, reverseordering(o))
        end
    end
    # Starting with an array which is not a heap in any order.
    let A = [13, 1, 14, 8, 10, 9, 7, 2, 5, 4, 12, 3, 6, 11]
        n = length(A)
        @test !isheap(A)
        @test !isheap(A, n)
        @test !isheap(A, Reverse)
        @test !isheap(A, FastForward)
        @test !isheap(A, FastReverse)
        B = heapify(A)
        @test isheap(B)
        @test !isheap(B, Reverse)
        B = heapify(A, Reverse, Int16(n))
        @test !isheap(B)
        @test isheap(B, Reverse)
        C = copy(A)
        B = heapify!(C)
        @test B === C
        @test isheap(B)
        B = heapify!(C, Reverse, Int16(n))
        @test B === C
        @test isheap(B, Reverse)
    end
end

@testset "Binary heaps          " begin
    n = 8
    for T in (Float64, Int)
        heap_test(T, BinaryHeap{T}(), n)
        heap_test(T, BinaryHeap{T}(FastMin), n)
        heap_test(T, BinaryHeap{T}(FastMax), n)
        heap_test(T, BinaryHeap{T}(SafeMin), n)
        heap_test(T, BinaryHeap{T}(SafeMax), n)
    end
end

@testset "Fast binary heaps     " begin
    n = 7
    for T in (Float32, Int16)
        heap_test(T, FastBinaryHeap{T}(), n)
        heap_test(T, FastBinaryHeap{T}(FastMin), n)
        heap_test(T, FastBinaryHeap{T}(FastMax), n)
        heap_test(T, FastBinaryHeap{T}(SafeMin), n)
        heap_test(T, FastBinaryHeap{T}(SafeMax), n)
    end
end

end # module
