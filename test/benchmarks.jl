module BenchmarkingQuickHeaps

using BenchmarkTools
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
    x = rand(T, n)
    xsav = copy(x)
    w = similar(x)
    width = 69
    for pass in 1:2
        for version in (:DataStructures,
                        :QuickHeaps)
            println("\nTimings for \"$version\" methods (T=$T, n=$n):")
            for order in (:(Base.Forward),
                          :(Base.Reverse),
                          :(DataStructures.FasterForward()),
                          :(DataStructures.FasterReverse()),
                          :(QuickHeaps.FastMin),
                          :(QuickHeaps.FastMax),)
                if pass == 1
                    print_with_arrow(" - $version.heapify!(..., $order)";
                                     width=width)
                    @btime $version.heapify!(copyto!($w, $x), $order); print();
                    @assert x == xsav # check that x was left unchanged
                    @assert @eval($version.isheap)(w, @eval($order))
                else
                    @eval($version.heapify!)(copyto!(w, x), @eval($order));
                    @assert x == xsav # check that x was left unchanged
                    print_with_arrow(" - $version.isheap(..., $order)";
                                     width=width)
                    @btime $version.isheap($w, $order); print();
                end
            end
        end
    end
end

if !isinteractive()
    runtests()
end

end # module
