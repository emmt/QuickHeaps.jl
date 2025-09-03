module BenchmarkingQuickHeaps

using BenchmarkTools
using ThreadPinning
using QuickHeaps
using DataStructures

using Base.Order: Forward, Reverse, Ordering

print_with_arrow(args...; kwds...) = print_with_arrow(stdout, args...; kwds...)
function print_with_arrow(io::IO, args...; width::Integer=70)
    msg = string(args...)
    len = max(2, width - 3 - length(msg))
    print(io, msg, " ", repeat("-", len), ">")
end

function runtests(T::Type = Float64, n::Int = 1000)
    local o, is_heap, mk_heap!
    pinthreads(:cores)
    x = rand(T, n)
    xsav = copy(x)
    w = similar(x)
    width = 69
    for pass in 1:2
        for version in (:DataStructures,
                        :QuickHeaps)
            mk_heap! = @eval $version.heapify!
            is_heap  = @eval $version.isheap
            println("\nTimings for \"$version\" methods (T=$T, n=$n):")
            for order in (:(Base.Order.Forward),
                          :(Base.Order.Reverse),
                          :(DataStructures.FasterForward()),
                          :(DataStructures.FasterReverse()),
                          :(QuickHeaps.FastMin),
                          :(QuickHeaps.FastMax),
                          :(QuickHeaps.TotalMin),
                          :(QuickHeaps.TotalMax),)
                o = @eval $order
                if pass == 1
                    print_with_arrow(" - $version.heapify!(..., $order)"; width=width)
                    @btime $(mk_heap!)(copyto!($w, $x), $o); print();
                    @assert x == xsav # check that x was left unchanged
                    @assert is_heap(w, o)
                else
                    f = @eval $version.heapify!
                    mk_heap!(copyto!(w, x), o);
                    @assert x == xsav # check that x was left unchanged
                    print_with_arrow(" - $version.isheap(..., $order)"; width=width)
                    @btime $(is_heap)($w, $o); print();
                end
            end
        end
    end
end

if !isinteractive()
    runtests()
end

end # module
