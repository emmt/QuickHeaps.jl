module TestingQuickHeapsUtilities

using Test

using QuickHeaps

@testset "Utilities             " begin
    let A = rand(Float32, 6)
        @test QuickHeaps.has_standard_linear_indexing(A) == true
        @test QuickHeaps.has_standard_linear_indexing(view(A, 2:3:6)) == true
    end

    @test QuickHeaps.is_one_based_unit_range(axes(rand(3), 1)) == true
    @test QuickHeaps.is_one_based_unit_range(1:4) == true
    @test QuickHeaps.is_one_based_unit_range(2:4) == false
    @test QuickHeaps.is_one_based_unit_range(1:2:5) == false

    @test QuickHeaps.require_one_based_indexing(1:2) == false

    let A = rand(Float32, 2)
        @test QuickHeaps.to_eltype(A, 11) isa Float32
        @test QuickHeaps.to_eltype(A, π) isa Float32
    end

    @test QuickHeaps.in_range(0, 3) == false
    @test QuickHeaps.in_range(1, 3) == true
    @test QuickHeaps.in_range(3, 3) == true
    @test QuickHeaps.in_range(4, 3) == false
    let A = collect(0:4)
        @test QuickHeaps.in_range(0, A) == false
        @test QuickHeaps.in_range(1, A) == true
        @test QuickHeaps.in_range(5, A) == true
        @test QuickHeaps.in_range(6, A) == false
    end
    let R = Base.OneTo(5)
        @test QuickHeaps.in_range(0, R) == false
        @test QuickHeaps.in_range(1, R) == true
        @test QuickHeaps.in_range(5, R) == true
        @test QuickHeaps.in_range(6, R) == false
    end
    let R = 0:4
        @test QuickHeaps.in_range(-1, R) == false
        @test QuickHeaps.in_range(0, R) == true
        @test QuickHeaps.in_range(4, R) == true
        @test QuickHeaps.in_range(5, R) == false
    end

    let x = nothing
        @test QuickHeaps.typename(x) isa String
        @test QuickHeaps.typename(x) == QuickHeaps.typename(typeof(x))
    end

    @test QuickHeaps.has_bad_values(1:2) == false
    @test QuickHeaps.has_bad_values([1.0,2.0]) == false
    @test QuickHeaps.has_bad_values([1.0,2.0,NaN]) == true

    @test_throws ArgumentError QuickHeaps.throw_argument_error("invald ", "argument")
    @test_throws DimensionMismatch QuickHeaps.throw_dimension_mismatch("not", " same dimensions")
end

end # module
