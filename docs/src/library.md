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
dequeue_pair!
enqueue!(::QuickHeaps.AbstractPriorityQueue, ::Any, ::Any)
```

## Orderings

```@docs
QuickHeaps.FastMin
QuickHeaps.FastMax
QuickHeaps.TotalMin
QuickHeaps.TotalMax
```

## Miscellaneous

The following non-exported methods may be needed for implementing new types of binary heap
or of priority queue. End-users probably not have to worry about these.

```@docs
QuickHeaps.has_bad_values
QuickHeaps.heap_index
QuickHeaps.storage
QuickHeaps.setroot!
```
