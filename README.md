# Versatile binary heaps and priority queues for Julia

[![Doc. Stable][doc-stable-img]][doc-stable]
[![Doc. Devel][doc-dev-img]][doc-dev]
[![License][license-img]][license-url]
[![Build Status][github-ci-img]][github-ci-url]
[![Build Status][appveyor-img]][appveyor-url]
[![Coverage][codecov-img]][codecov-url]
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)

`QuickHeaps` is a [Julia][julia-url] package providing versatile [binary
heaps](https://en.wikipedia.org/wiki/Binary_heap) and [priority
queues](https://en.wikipedia.org/wiki/Priority_queue). These data structures are more
flexible and are faster than those provided by [`DataStructures`][datastructures-url].

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
   be ignored. A better and less radical solution is to implement similar but faster
   functions to compare values.

2. Some non-exported methods may be very useful if they can be safely called.

This short note is bout dealing with these two points.


### Speed up sorting

Sorting algorithms in Julia rely on `Base.Order.lt(o,x,y)` to check whether `x` is *strictly
before* `y` according to the ordering specified in `o` (the letters `lt` stands for *less
than*). Most (all?) Julia sorting methods have an `order` keyword to specify the ordering.
By default, ordering is `Base.Order.Forward` which is the singleton of type
`Base.Order.ForwardOrdering`. Another usual choice is to take `Base.Order.Reverse`. In
short, when sorting, you are using the following definitions in module `Base.Order`:

```julia
const Forward = ForwardOrdering()
const Reverse = ReverseOrdering()
lt(o::ForwardOrdering, a, b) = isless(a,b)
lt(o::ReverseOrdering, a, b) = lt(o.fwd,b,a)
```

So it turns out that, `isless` is eventually called, not the operator `<`. For integers
`isless(x,y)` and `x < y` are the same (at least as far as execution time is concerned) but
for floating-point values, `isless` takes care of NaN's which involves overheads and this
may strongly impact the execution time of sorting algorithms. A simple mean to speedup these
comparisons consists in avoiding branching by replacing logical operators (`&&` and `||`) by
bitwise operators (`&` and `|`). This trick is exploited by `TotalMin`, the order used by
default in `QuickHeaps`, which yields the same [*total
order*](https://en.wikipedia.org/wiki/Total_order) as `Base.Order.Forward` but about twice
faster. `QuickHeaps` also provides `FastMin` order which is a bit faster than `TotalMin` but
which leaves the order of `NaN` values undefined. For numbers, the implementation of
`TotalMin` and `FastMin` is pretty simple:

```julia
struct TotalMinOrdering <: Base.Order.Ordering end
const TotalMin = TotalMinOrdering()
Base.Order.lt(::TotalMin, a::T, b::T) where {T<:Number} =
    (! isnan(a)) & (isnan(b) | (a < b))
```

Then just use keyword `order=TotalMin` in calls to Julia sort methods to benefit from a
speed-up factor of almost 2, for free! Other orders to consider are `TotalMax`,
`reverse(TotalMin)`, and `reverse(TotalMax)`. Before Julia 1.8, replace `reverse(o)` by
`Base.Order.ReverserOrdering(o)` for order `o`.

Package [`DataStructures`](https://github.com/JuliaCollections/DataStructures.jl) provides
`DataStructures.FasterForward()` and `DataStructures.FasterReverse()` orders which are the
same as `QuickHeaps.FastMin` and `QuickHeaps.FastMax`. Hence, fast for floating-point values
but leaving undefined the order `NaN` values.


### Benchmarking binary heaps

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
