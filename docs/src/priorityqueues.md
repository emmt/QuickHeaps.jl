# Priority queues

[Priority queues](https://en.wikipedia.org/wiki/Priority_queue) are partially ordered
dynamic lists of so-called *nodes* which are key-value pairs. Priority queues are designed
so that updating the list of stored nodes while maintaining the ordering and retrieving or
extracting the node of highest priority are efficient operations.

Priority queues provided by `QuikHeaps` are similar to dictionaries (or to arrays) with the
additional feature of maintaining an ordered structure so that getting the node of highest
priority costs `O(1)` operations while changing the priority of a node or pushing a node
only costs `O(log(n))` operations with `n` the length of the queue.


## Building priority queues

In `QuikHeaps`, priority queues combine a [binary heap](#Binary-heaps) to store the
partially sorted list of nodes and another structure to associate keys and nodes. There are
two possibilities depending on the kind of keys.

### Versatile priority queues

`QuikHeaps` provides versatile priority queues which use a dictionary to associate keys with
nodes and thus impose no restrictions on the type of the keys. To build a versatile priority
queue, call the [`PriorityQueue`](@ref) constructor:

```julia
Q = PriorityQueue{K,V}([o=Forward,] T=Node{K,V})
```

where optional parameter `o::Ordering` specifies the ordering for deciding the priority of
values while optional parameter `T<:AbstractNode{K,V}` specifies the type of the nodes with
`K` and `V` the respective types of the keys and of the values. Type parameters `K` and `V`
may be omitted if the node type `T` is specified.


### Fast priority queues

If keys are analogous to indices in some array, the key-node association can be realized by
a regular array which is faster than a dictionary as used by versatile priority queues. To
build a fast priority queue with keys indexing an array of dimensions `dims...`, call the
[`FastPriorityQueue`](@ref) constructor:

```julia
Q = FastPriorityQueue{V}([o=Forward,] [T=Node{Int,V},] dims...)
```

where `o::Ordering` specifies the ordering of values in the priority queue,
`T<:AbstractNode{Int,V}` is the type of the nodes depending on `V` the type of the values
encoding the priority. Type parameter `V` may be omitted if the node type `T` is specified.

The keys in this kind of priority queue are the linear or Cartesian indices in an array of
size `dims...`. For example, if `dims = (3,4,5)`, then all the following expressions refer
to the same key:

```julia
Q[44]
Q[2,3,4]
Q[CartesianIndex(2,3,4)]
```

The storage of a fast priority queue requires `prod(dims...)*sizeof(Int) + n*sizeof(T)`
bytes with `n` enqueued nodes.


## Common methods for priority queues

In `QuikHeaps`, priority queues have a common interface which is described here.

The first thing to do with a freshly created priority queue is to populate it. To enqueue
key `k` with priority `v` in priority queue `Q`, all the following is equivalent:

```julia
Q[k] = v
push!(Q, k => v)
enqueue!(Q, k => v)
enqueue!(Q, k, v)
```

Note that key `k` may already exists in `Q`; in this case, the priority associated with the
key is updated and the queue reordered if necessary in, at worse, `O(log(n))` operations.
This is generally faster than first deleting the key and then enqueuing the key with the new
priority.

To extract the node of highest priority out of the queue `Q` and get its key `k` and,
possibly, its priority `v`, call one of:

```julia
k = dequeue!(Q)
k, v = pop!(Q)
```

Methods [`dequeue_pair!`](@ref) and [`dequeue_node!`](@ref) also extract the root node out
of a priority queue and return it as a `Pair` or as as a node of the type used by the queue.

To just examine the node of highest priority, call one of:

```julia
k, v = peek(Q)
k, v = first(Q)
```

Like the `dequeue!` method, the `peek` method may also be called with a type argument.

A priority queue `Q` behaves like a dictionary:

```julia
length(Q)         # yields number of nodes
isempty(Q)        # yields whether priority queue is empty
empty!(Q)         # empties priority queue
keytype(Q)        # yields key type `K`
valtype(Q)        # yields value type `V`
Q[v]              # yields the value of key `k`
get(Q, k, def)    # query value at key, with default
Q[k] = v          # set value `v` of key `k`
push!(Q, k => v)  # idem.
haskey(Q, k)      # yields whether key `k` exists
delete!(Q, k)     # delete node at key `k`
```

Note that the syntax `Q[k]` throws an exception if key `k` does not exists in `Q`.

Finally, there are different ways to iterate on the (unordered) contents of a priority queue
`Q`:

```julia
keys(Q)                  # yields iterator over keys
vals(Q)                  # yields iterator over values
for (k,v) in Q; ...; end # loop over key-value pairs
```

Note that these iterators yield nodes in their storage order which is not necessarily that
of their priority. The order is however the same for these iterators.


## Priority order

How are ordered the nodes is completely customizable by specializing the
[`QuickHeaps.lt`](@ref) function with the following signature:

```julia
QuickHeaps.lt(o::OrderingType, x::T, y::T) where {T<:NodeType}
```

which shall yield whether node `x` has (strictly) higher priority than node `y` in the queue
and where `OrderingType` and `NodeType <: [QuickHeaps.AbstractNode](@ref)` are the
respective types of the ordering and of the nodes of the priority queue.

For the default node type, `QuickHeaps.Node{K,V}`, the implementation is:

```julia
QuickHeaps.lt(o::Ordering, x::T, y::T) where {T<:QuickHeaps.Node} =
    Base.lt(o, QuickHeaps.get_val(x), QuickHeaps.get_val(y))
```

where [`QuickHeaps.get_val(x)`](@ref QuickHeaps.get_val) yields the value of node `x`. In
other words, nodes are sorted by their value according to ordering `o`.
