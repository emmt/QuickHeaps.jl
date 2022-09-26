    function check_contents(A::Union{AbstractPriorityQueue{K,V},
                                     AbstractDict{K,V}},
                            B::Union{AbstractPriorityQueue{K,V},
                                     AbstractDict{K,V}}) where {K,V}
        set = Set(keys(A))
        for k in keys(B)
            push!(set, k)
        end
        flag = true
        for k in set
            flag &= (haskey(A) && haskey(B))
            flag &= (A[k] == B[k])
        end
        return flag
    end

#IteratorEltype(pq::AbstractPriorityQueue) = IteratorEltype(typepof(pq))
#IteratorEltype(::Type{<:AbstractPriorityQueue}) = HasEltype()
#eltype(pq::AbstractPriorityQueue) = eltype(typepof(pq))
#eltype(::Type{<:AbstractPriorityQueue{K,V}}) where {K,V} = V
#
#IteratorSize(pq::AbstractPriorityQueue) = IteratorSize(typepof(pq))
#IteratorSize(::Type{<:AbstractPriorityQueue}) = HasLength()





function PriorityQueue(dict::Dict{K,V},
                       o::O = DefaultOrdering,
                       ::Type{T} = Node{K,V}) where {K,V,T<:AbstractNode{K,V},
                                                     O<:Ordering}
    # Build the binary heap backing the storage of the nodes of the priority
    # queue.
    n = length(dict)
    A = Array{T}(undef, n)
    i = 0
    for (k, v) in dict
        i += 1
        A[i] = T(k, v)
    end
    heapify!(o, A)

    # Build the index ot the priority queue.
    I = Dict{Int,V}()
    @inbounds for i in 1:n
        k = getkey(A[i])
        I[k] = i
    end
    return PriorityQueue{K,V,T,O}(o, A, I)
end

const TrailingPairs{K,V} = Vararg{Pair{K,V}}

for type in (:AbstractDict, :TrailingPairs)
    var = (type === :AbstractDict ? :dict : :pairs)
    expr = (type === :AbstractDict ? var : :($var...))
    @eval begin
        PriorityQueue($var::$type{K,V}) where {K,V} =
            PriorityQueue(Node{K,V}, $expr)
        PriorityQueue{K}($var::$type{<:Any,V}) where {K,V} =
            PriorityQueue(Node{K,V}, $expr)
        PriorityQueue{K,V}($var::$type) where {K,V} =
            PriorityQueue(Node{K,V}, $expr)

        PriorityQueue(T::Type{<:AbstractNode{K,V}}, $var::$type) where {K,V} =
            PriorityQueue(DefaultOrdering, T, $expr)
        PriorityQueue{K}(T::Type{<:AbstractNode{K,V}}, $var::$type) where {K,V} =
            PriorityQueue(DefaultOrdering, T, $expr)
        PriorityQueue{K,V}(T::Type{<:AbstractNode{K,V}}, $var::$type) where {K,V} =
            PriorityQueue(DefaultOrdering, T, $expr)

        PriorityQueue(o::Ordering, $var::$type{K,V}) where {K,V} =
            PriorityQueue(o, Node{K,V}, $expr)
        PriorityQueue{K}(o::Ordering, $var::$type{<:Any,V}) where {K,V} =
            PriorityQueue(o, Node{K,V}, $expr)
        PriorityQueue{K,V}(o::Ordering, $var::$type) where {K,V} =
            PriorityQueue(o, Node{K,V}, $expr)

        PriorityQueue{K}(o::Ordering, T::Type{<:AbstractNode{K,V}}, $var::$type{K,V}) where {K,V} =
            PriorityQueue(o, T, $expr)
        PriorityQueue{K,V}(o::Ordering, T::Type{<:AbstractNode{K,V}}, $var::$type) where {K,V} =
            PriorityQueue(o, T, $expr)
    end

end

PriorityQueue(args::AbstractNode...) =
    PriorityQueue(DefaultOrdering, args...)

PriorityQueue(o::Ordering, args::T...) where {K,V,T<:AbstractNode{K,V}} =
    PriorityQueue(o, Iterators.map(T, args))

PriorityQueue(o::Ordering, T::Type{<:AbstractNode{K,V}}, dict::AbstractDict) where {K,V} =
    PriorityQueue(o, Iterators.map(T, dict))

PriorityQueue(o::Ordering, T::Type{<:AbstractNode{K,V}}, args::Pair...) where {K,V} =
    PriorityQueue(o, Iterators.map(T, args))

PriorityQueue(o::Ordering, T::Type{<:AbstractNode{K,V}}, args::AbstractNode...) where {K,V} =
    PriorityQueue(o, Iterators.map(T, args))

function PriorityQueue(A::AbstractVector{<:AbstractNode},
                       o::Ordering = DefaultOrdering)
    return PriorityQueue(o, A)
end

function PriorityQueue(o::Ordering, A::AbstractVector{<:AbstractNode})
    require_one_based_indexing(A)
    n = length(A)
    return to_priority_queue!(o, copyto!(Vector{T}(undef, n), 1, A, 1, n))
end

function PriorityQueue(itr::Base.Generator{<:Any,<:AbstractNode},
                       o::Ordering = DefaultOrdering)
    return PriorityQueue(o, itr)
end

function PriorityQueue(o::Ordering,
                       itr::Base.Generator{<:Any,T}) where {K,V,
                                                            T<:AbstractNode{K,V}}
    return to_priority_queue!(o, collect(T, itr)::Vector{T})
end

function to_priority_queue!(o::O, A::Vector{T}) where {K,V,O<:Ordering,
                                                       T<:AbstractNode{K,V}}
    # Sort the array backing the storage of the binary heap and build the index
    # ot the priority queue.
    heapify!(o, A)
    I = Dict{Int,V}()
    @inbounds for i in 1:length(A)
        k = getkey(A[i])
        I[k] = i
    end
    return PriorityQueue{K,V,T,O}(o, A, I)
end

function PriorityQueue(o::Ordering,
                       T::Type{<:AbstractNode{K,V}},
                       dict::Dict{K,V}) where {K,V}
    return PriorityQueue(dict, o, T)
end

function PriorityQueue(T::Type{<:AbstractNode{K,V}},
                       o::Ordering,
                       dict::Dict{K,V}) where {K,V}
    return PriorityQueue(dict, o, T)
end

function PriorityQueue(dict::Dict{K,V},
                       T::Type{<:AbstractNode{K,V}},
                       o::Ordering = DefaultOrdering) where {K,V}
    return PriorityQueue(dict, o, T)
end

function PriorityQueue(o::Ordering,
                       T::Type{<:AbstractNode{K,V}},
                       pairs::Pair{K,V}...) where {K,V}
    return PriorityQueue(Dict{K,V}(pairs...), o, T)
end
