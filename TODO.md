- Add `@inbounds` and benchmark priority queues.

- Extend `maximum` and `minimum` for binary heaps.

- Remove `push!(heap,())`, implement `push!(heap)`.

- Implement `pushpop!` and `heapreplace!` (see
  https://en.wikipedia.org/wiki/Binary_heap).

- Constructor `PriorityQueue(o=default_ordering(), pairs::Pairs{K,V}...)` idem with a
  dictionary.

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

- Change `show` method for `AbstractPriorityQueue` to reflect that.

- Allow for directly storing pairs in `AbstractPriorityQueue`?

- Implement `merge`, `merge!`, and `reverse`:
  ```julia
  reverse(h::BinaryHeap) = BinaryHeap(copy(values(h), 1:lenght(h)), reverse(ordering(h)))
  reverse(pq::PriorityQueue{K,V,O,T}) where {K,V,O,T} =
      merge!(PriorityQueue{K,V}(reverse(ordering(pq)), T), pq)
  reverse(pq::FastPriorityQueue{V,N,O,T}) where {V,N,O,T} =
      merge!(FastPriorityQueue{V,N}(reverse(ordering(pq)), T), size(index(pq)); pq)
  ...
  ```
