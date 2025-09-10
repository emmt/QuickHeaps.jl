# Priority queues

[Priority queues](https://en.wikipedia.org/wiki/Priority_queue) are partially ordered
dynamic lists whose entries are key-value pairs. Priority queues are designed so that
updating the list of stored entries while maintaining the ordering and retrieving or
extracting the entry of highest priority are efficient operations.

Priority queues provided by `QuikHeaps` are similar to dictionaries (or to arrays) with the
additional feature of maintaining an ordered structure so that getting the entry of highest
priority costs `O(1)` operations while changing the priority of an entry or pushing an entry
only costs `O(log(n))` operations with `n` the number of entries in the queue.


## Building priority queues

In `QuikHeaps`, priority queues combine a [binary heap](#Binary-heaps) to store the
partially sorted list of entries and another structure to associate keys and entries. There
are two possibilities depending on the kind of keys.

### Versatile priority queues

`QuikHeaps` provides versatile priority queues which use a dictionary to associate keys with
values and thus impose no restrictions on the type of the keys. To build a versatile priority
queue, call the [`PriorityQueue`](@ref) constructor:

```julia
Q = PriorityQueue{K,V}(o=TotalMin)
```

where type parameters `K` and `V` are the respective types of the keys and of the values
while optional parameter `o::Ordering` specifies the ordering for deciding the priority of
values.


### Fast priority queues

If keys are analogous to indices in some array, the key-value association can be realized by
a regular array which is faster than a dictionary as used by versatile priority queues. To
build a fast priority queue with keys indexing an array of dimensions `dims...`, call the
[`FastPriorityQueue`](@ref) constructor:

```julia
Q = FastPriorityQueue{V}([o=TotalMin,] dims...)
```

where type parameter `V` is the type of the values, optional `o::Ordering` specifies the
ordering of values in the priority queue, and arguments `dims...` specify the array shape.

The keys in this kind of priority queue are the linear or Cartesian indices in an array of
size. For example, if `dims = (3,4,5)`, then all the following expressions refer
to the same key:

```julia
Q[44]
Q[2,3,4]
Q[CartesianIndex(2,3,4)]
Q[CartesianIndex(2,3),4]
```

The keys of fast priority queues are however stored as linear indices, hence:

```julia
getkey(Q, 44, missing)
getkey(Q, CartesianIndex(2,3,4), missing)
```

would both yield `44` if this key is defined or `missing` otherwise.

For `n` enqueued entries, the storage of a fast priority queue requires about
`sizeof(Int)*(n + prod(dims...)) + n*sizeof(V)` bytes.


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

To extract the entry of highest priority out of the queue `Q` and get its key `k` and,
possibly, its priority value `v`, call one of:

```julia
k = dequeue!(Q)
k, v = pop!(Q)
k, v = dequeue_pair!(Q)
```

Like `pop!`, [`dequeue_pair!`](@ref) extracts the root entry out of a priority queue
and return it as a key-value `Pair`.

To just examine the entry of highest priority, call one of:

```julia
k, v = peek(Q)
k, v = first(Q)
```

A priority queue `Q` behaves like a dictionary:

```julia
length(Q)              # get number of entries
isempty(Q)             # whether priority queue is empty
empty!(Q)              # make priority queue empty
keytype(Q)             # get key type `K`
valtype(Q)             # get value type `V`
Q[v]                   # get the value of key `k`
get(Q, k, def)         # query value at key, with default
getkey(Q, k, def)      # query key corresponding to `k` with default
Q[k] = v               # set value `v` of key `k`
push!(Q, k => v)       # idem.
haskey(Q, k)           # whether key `k` exists
delete!(Q, k)          # delete entry at key `k`
Base.Order.Ordering(Q) # get order `o` of priority queue `Q`
```

Note that the syntax `Q[k]` throws an exception if key `k` does not exists in `Q`.

Finally, there are different ways to iterate on the (unordered) contents of a priority queue
`Q`:

```julia
for k in keys(Q); ...; end   # loop over keys
for v in values(Q); ...; end # loop over values
for (k,v) in Q; ...; end     # loop over key-value pairs
```

Note that these iterators yield entries in their storage order which is not necessarily that
of their priority. The order is however the same for these iterators.
