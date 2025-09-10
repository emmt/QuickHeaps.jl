module TestingQuickHeapsUtilities

using Test

using QuickHeaps

@testset "Orders                " begin
    vals = (-Inf, -1, 0, 2, π, +Inf, NaN, missing)
    @testset "Base.Order.lt(TotalMin, $x, $y)" for x in vals, y in vals
        @test Base.Order.lt(TotalMin, x, y) == isless(x, y)
    end
    @testset "Base.Order.lt(TotalMax, $x, $y)" for x in vals, y in vals
        if x === missing || isnan(x) || y === missing || isnan(y)
            @test Base.Order.lt(TotalMax, x, y) == Base.Order.lt(TotalMin, x, y)
        else
            @test Base.Order.lt(TotalMax, x, y) == Base.Order.lt(TotalMin, y, x)
            @test Base.Order.lt(FastMax, x, y) == (x > y)
        end
    end
    @testset "Base.Order.lt(FastMin, $x, $y)" for x in vals, y in vals
        if !(x === missing || isnan(x) || y === missing || isnan(y))
            @test Base.Order.lt(FastMin, x, y) == (x < y)
        end
    end
    @testset "Base.Order.lt(FastMax, $x, $y)" for x in vals, y in vals
        if !(x === missing || isnan(x) || y === missing || isnan(y))
            @test Base.Order.lt(FastMax, x, y) == (x > y)
        end
    end
end

@testset "Utilities             " begin
    let A = rand(Float32, 6)
        @test QuickHeaps.has_standard_linear_indexing(A) == true
        @test QuickHeaps.has_standard_linear_indexing(view(A, 2:3:6)) == true
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

    @test QuickHeaps.has_bad_values(1:2) == false
    @test QuickHeaps.has_bad_values([1.0,2.0]) == false
    @test QuickHeaps.has_bad_values([1.0,2.0,NaN]) == true

    @test_throws ArgumentError QuickHeaps.throw_argument_error("invalid ", "argument")
    @test_throws DimensionMismatch QuickHeaps.throw_dimension_mismatch("not", " same dimensions")
end

end # module
