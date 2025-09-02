# Reference

The following reproduces the in-lined documentation about types and methods of the
[`QuickHeaps`](https://github.com/emmt/QuickHeaps.jl) package. This documentation is also
available from the REPL by typing `?` followed by the name of a method or a type.

## Binary Heaps

```@docs
QuickHeaps.AbstractBinaryHeap
QuickHeaps.BinaryHeap
QuickHeaps.FastBinaryHeap
QuickHeaps.heapify
QuickHeaps.heapify!
QuickHeaps.heapify_down!
QuickHeaps.heapify_up!
QuickHeaps.isheap
QuickHeaps.unsafe_heapify_down!
QuickHeaps.unsafe_heapify_up!
QuickHeaps.unsafe_grow!
QuickHeaps.unsafe_shrink!
```

## Priority Queues

```@docs
QuickHeaps.AbstractPriorityQueue
QuickHeaps.PriorityQueue
QuickHeaps.FastPriorityQueue
dequeue!(::QuickHeaps.AbstractPriorityQueue)
dequeue_node!
dequeue_pair!
enqueue!(::QuickHeaps.AbstractPriorityQueue, ::Any, ::Any)
```

## Nodes

```@docs
QuickHeaps.AbstractNode
QuickHeaps.Node
QuickHeaps.get_key
QuickHeaps.get_val
```

## Orderings

```@docs
QuickHeaps.FastForwardOrdering
QuickHeaps.default_ordering
```

## Miscellaneous

The following non-exported methods may be needed for implementing new types of binary heap
or of priority queue. End-users probably not have to worry about these.

```@docs
QuickHeaps.has_bad_values
QuickHeaps.has_standard_linear_indexing
QuickHeaps.heap_index
QuickHeaps.in_range
QuickHeaps.is_one_based_unit_range
QuickHeaps.linear_index
QuickHeaps.lt
QuickHeaps.nodes
QuickHeaps.index
QuickHeaps.storage
QuickHeaps.ordering
QuickHeaps.setroot!
QuickHeaps.to_key
QuickHeaps.to_node
QuickHeaps.to_val
QuickHeaps.typename
```
