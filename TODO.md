- Add `@inbounds` and benchmark priority queues.

- Extend `maximum` and `minimum` for binary heaps.

- Remove `push!(heap,())`, implement `push!(heap)`.

- Implement `pushpop!` and `heapreplace!` (see
  https://en.wikipedia.org/wiki/Binary_heap).

- Provide aliases `MinHeap`, `MaxHeap`, `FastMinHeap`, and `FastMaxHeap`.

- Add a switch to automatically cope with NaN's.

- `enqueue!(Val(:up), pq, k => v)` and `enqueue!(Val(:down), pq, k => v)`

- Priority queue index can be any sub-type of `AbstractDict` of `AbstractArray`.

- Deal with non 1-based indices in binary heaps.

- Ensure type-stability of pairs (`Pair{K,V}`) returned by priority queues.

- Have a `wrap!(BinaryHeap,vals)` method that shares the vector of values while
  the constructor always copies the vector of values.

- For priority queues: extend `pairs`

- Remove unused `heapify_down!`, `heapify_up!`, and
  `require_one_based_indexing`.

- Change ordering of type parameters for `AbstractPriorityQueue` to
  `{K,V,O,T<:AbstractNode{K,V}}`. Change `show` method for
  `AbstractPriorityQueue` to reflect that.

- Allow for directly storing pairs in `AbstractPriorityQueue`?
