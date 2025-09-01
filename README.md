# Versatile binary heaps and priority queues for Julia

[![Doc. Stable][doc-stable-img]][doc-stable]
[![Doc. Devel][doc-dev-img]][doc-dev]
[![License][license-img]][license-url]
[![Build Status][github-ci-img]][github-ci-url]
[![Build Status][appveyor-img]][appveyor-url]
[![Coverage][codecov-img]][codecov-url]
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)

`QuickHeaps` is a small [Julia][julia-url] package providing versatile [binary
heaps](https://en.wikipedia.org/wiki/Binary_heap) and [priority
queues](https://en.wikipedia.org/wiki/Priority_queue). These data structures are more
flexible and may be quite significantly faster than those provided by
[`DataStructures`][datastructures-url].

## Installation

The easiest way to install `QuickHeaps` is to use [Julia's package
manager](https://pkgdocs.julialang.org/):

```julia
using Pkg
pkg"add QuickHeaps"
```

## Documentation

Documentation is available for different versions of `QuickHeaps`:

- [last release][doc-stable];

- [development version][doc-stable].


## Speed up and strengthen sorting algorithms

The sorting algorithms in Julia are very powerful but have some issues (for me):

1. Sorting algorithms involve lots of comparisons and could be much faster if we know that
   there are no NaN's in the arrays to sort or if we assume (at our own risk) that they can
   be ignored.

2. Some non-exported methods may be very useful if they can be safely called.

This short note is bout dealing with these two points.


### Speed up sorting

Sorting algorithms in Julia rely on `Base.Order.lt(o,x,y)` to check whether `x` is *before*
`y` according to the ordering specified in `o` (the letters `lt` stands for *less than*).
Most (all?) Julia sorting methods have an `order` keyword to specify the ordering. By
default, ordering is `Base.Order.Forward` which is the singleton of type
`Base.Order.ForwardOrdering`. Another usual choice is to take `Base.Order.Reverse`. In
short, when sorting, you are using the following definitions:

```julia
const Forward = ForwardOrdering()
const Reverse = ReverseOrdering()
lt(o::ForwardOrdering, a, b) = isless(a,b)
lt(o::ReverseOrdering, a, b) = lt(o.fwd,b,a)
```

So it turns out that, `isless` is eventually called, not the operator `<`. For integers
`isless(x,y)` and `x < y` are the same (at least as far as execution time is concerned) but
for floating-point values, `isless` takes care of NaN's which involves overheads and this
may strongly impact the execution time of sorting algorithms.

Simple means to ignore NaN's in sorting consists in defining your own ordering types and
extend the `Base.Order.lt` method, this is pretty simple:

```julia
using Base: Ordering, ReverseOrdering
struct FastForwardOrdering <: Ordering end
const FastForward = FastForwardOrdering()
const FastReverse = ReverseOrdering(FastForward)

import Base: lt
lt(::FastForwardOrdering, a, b) = a < b
```

Then just use keyword `order=FastForward` or `order=FastReverse` in calls to sort methods to
benefit from a speed-up factor between 2 or 3. Almost for free! The very same trick has been
implemented in the [`DataStructures`](https://github.com/JuliaCollections/DataStructures.jl)
package with `DataStructures.FasterForward()` and `DataStructures.FasterReverse()`.

Checking that an array has NaN's can be checked in linear time, that is `O(n)`, for arrays
of `n` floating-point values by the following method:

```julia
function has_nans(A::AbstractArray{<:AbstractFloat})
    flag = false
    @inbounds @simd for i in eachindex(A)
        flag |= isnan(A[i])
    end
    return flag
end
```

where short-circuit has been purposely avoided to exploit SIMD optimization. The rationale
is that if you are mostly interested in arrays with no NaN's, you expect that the entire
array be checked. A simple benchmark follows:

```julia
using BenchmarkTools
x = rand(1000);
@btime has_nans($x) # ----> 119.054 ns (0 allocations: 0 bytes)
```

which is much shorter than the time it takes to heapify the array. This test could be
applied to arrays of floating-point values to choose between fast/slow ordering. This would
not change the behavior of the sorting methods but would significantly reduce the execution
time most of the time.

For integer-valued arrays, it takes `O(1)` time to check for NaN's:

```julia
has_nans(A::AbstractArray{<:Integer}) = false
```

An additional speed-up by a factor between 1.5 and 2 is achievable by proper use of
`@inline`, `@inbounds` and `@propagate_inbounds` macros in the code implementing the sorting
algorithms. This however requires to modify existing code. This is what is done in
[`QuickHeaps`](https://github.com/emmt/QuickHeaps.jl).


### Application to binary heaps

As an illustration of the above discussion, below is the output of a small benchmark ran by:

```julia
julia --project test/benchmarks.jl
```

with Julia 1.6.3 on an AMD Ryzen Threadripper 2950X 16-Core Processor:

```
Timings for "DataStructures" methods (T=Float64, n=1000):
 - DataStructures.heapify!(..., Base.Forward) ---------------------> 7.478 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., Base.Reverse) ---------------------> 7.268 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., DataStructures.FasterForward()) ---> 3.444 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., DataStructures.FasterReverse()) ---> 3.428 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., QuickHeaps.FastMin) ---------------> 3.413 μs (0 allocations: 0 bytes)
 - DataStructures.heapify!(..., QuickHeaps.FastMax) ---------------> 3.428 μs (0 allocations: 0 bytes)

Timings for "QuickHeaps" methods (T=Float64, n=1000):
 - QuickHeaps.heapify!(..., Base.Forward) -------------------------> 4.852 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., Base.Reverse) -------------------------> 4.506 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., DataStructures.FasterForward()) -------> 1.655 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., DataStructures.FasterReverse()) -------> 1.658 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., QuickHeaps.FastMin) -------------------> 1.637 μs (0 allocations: 0 bytes)
 - QuickHeaps.heapify!(..., QuickHeaps.FastMax) -------------------> 1.658 μs (0 allocations: 0 bytes)

Timings for "DataStructures" methods (T=Float64, n=1000):
 - DataStructures.isheap(..., Base.Forward) -----------------------> 1.910 μs (0 allocations: 0 bytes)
 - DataStructures.isheap(..., Base.Reverse) -----------------------> 1.932 μs (0 allocations: 0 bytes)
 - DataStructures.isheap(..., DataStructures.FasterForward()) -----> 563.027 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., DataStructures.FasterReverse()) -----> 575.110 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., QuickHeaps.FastMin) -----------------> 575.087 ns (0 allocations: 0 bytes)
 - DataStructures.isheap(..., QuickHeaps.FastMax) -----------------> 573.750 ns (0 allocations: 0 bytes)

Timings for "QuickHeaps" methods (T=Float64, n=1000):
 - QuickHeaps.isheap(..., Base.Forward) ---------------------------> 1.820 μs (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., Base.Reverse) ---------------------------> 1.821 μs (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., DataStructures.FasterForward()) ---------> 381.527 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., DataStructures.FasterReverse()) ---------> 383.847 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., QuickHeaps.FastMin) ---------------------> 378.627 ns (0 allocations: 0 bytes)
 - QuickHeaps.isheap(..., QuickHeaps.FastMax) ---------------------> 384.631 ns (0 allocations: 0 bytes)
```

These timings show the gain in speed for `heapify!` by using `<` instead of `isless` by a
factor of 2.3 for the binary heap implemented by `DataStructures` and by a factor of 3.2 for
the binary heap implemented by `QuickHeaps`.

These timings also show that `heapify!` in `QuickHeaps` is faster than in `DataStructures`
by a factor greater than 1.5 with standard orderings and by a factor better than 2 with
faster orderings.


[julia-url]: https://julialang.org/
[datastructures-url]: https://github.com/JuliaCollections/DataStructures.jl

[license-url]: ./LICENSE.md
[license-img]: http://img.shields.io/badge/license-MIT-brightgreen.svg?style=flat

[doc-stable]: https://emmt.github.io/QuickHeaps.jl/stable
[doc-dev]: https://emmt.github.io/QuickHeaps.jl/dev

[doc-stable-img]: https://img.shields.io/badge/docs-stable-blue.svg
[doc-dev-img]: https://img.shields.io/badge/docs-dev-blue.svg

[github-ci-img]: https://github.com/emmt/QuickHeaps.jl/actions/workflows/CI.yml/badge.svg?branch=master
[github-ci-url]: https://github.com/emmt/QuickHeaps.jl/actions/workflows/CI.yml?query=branch%3Amaster

[appveyor-img]: https://ci.appveyor.com/api/projects/status/github/emmt/QuickHeaps.jl?branch=master
[appveyor-url]: https://ci.appveyor.com/project/emmt/QuickHeaps-jl/branch/master

[codecov-img]: http://codecov.io/github/emmt/QuickHeaps.jl/coverage.svg?branch=master
[codecov-url]: http://codecov.io/github/emmt/QuickHeaps.jl?branch=master
