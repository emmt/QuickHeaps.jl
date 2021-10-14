module TestingFasterHeaps

using Test, FasterHeaps
using FasterHeaps:
    AbstractHeap, FastMinOrdering,
    FastMin, FastMax, SafeMin, SafeMax,
    isheap, ordering

using Base.Order: Ordering, ReverseOrdering, ForwardOrdering

is_max_ordering(x) = !is_min_ordering(x)
is_min_ordering(o::Ordering) = is_min_ordering(typeof(o))
is_min_ordering(::AbstractHeap{<:Any,O}) where {O} = is_min_ordering(O)
is_min_ordering(::Type{<:FastMinOrdering}) = true
is_min_ordering(::Type{<:ForwardOrdering}) = true
is_min_ordering(::Type{<:ReverseOrdering{O}}) where {O} = is_max_ordering(O)

function is_sorted(o::Base.Ordering, x::AbstractVector)
    flag = false
    for i in 2:length(x)
        flag |= Base.lt(o, x[i], x[i-1])
    end
    return !flag
end

function heap_test(T::Type, h::AbstractHeap, n::Integer = 15)
    @test eltype(h) === T
    @test size(h) == (length(h),)
    @test IndexStyle(h) == IndexLinear()
    empty!(h)
    @test isempty(h)
    @test isheap(h; check=true)
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

@testset "Array as heap" begin
    n = 15
    for T in (Float32, Int)
        heap_test(T, Heap{T}(), 15)
        heap_test(T, Heap{T}(FastMin), 15)
        heap_test(T, Heap{T}(FastMax), 15)
        heap_test(T, Heap{T}(SafeMin), 15)
        heap_test(T, Heap{T}(SafeMax), 15)
    end
end

@testset "Fast heap" begin
    n = 15
    for T in (Float32, Int)
        heap_test(T, FastHeap{T}(), 15)
        heap_test(T, FastHeap{T}(FastMin), 15)
        heap_test(T, FastHeap{T}(FastMax), 15)
        heap_test(T, FastHeap{T}(SafeMin), 15)
        heap_test(T, FastHeap{T}(SafeMax), 15)
    end
end

end # module
