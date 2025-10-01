- Provide a better designed API (public but not exported) to implement custom
  binary heaps and priority queues.

- `unset` entries in `FastBinaryHeaps` using the
  [`UnsetIndex.jl`](https://github.com/emmt/UnsetIndex.jl) package.

- Avoid storing the keys twice in `PriorityQueue`.

- Use `ZippedVector{Pair{K,V}}` instead of `Vector{Pair{K,V}}` to store the heap
  in `PriorityQueue`: may be faster and may use less memory.

- Use `Memory` instead of `Vector` in some structures.

- Optimize `has_standard_indexing`: `IndexLinear` must be the `IndexStyle` and 1-based
  linear indices can only be false for 1-dimensional arrays.

- `setroot!` for priority queues.

- Add `@inbounds` and benchmark priority queues.

- Extend `maximum` and `minimum` for binary heaps.

- Remove `push!(heap,())`, implement `push!(heap)`.

- Implement `popat!` and `popfirst!`.

- Implement `pushpop!` and `heapreplace!` (see
  https://en.wikipedia.org/wiki/Binary_heap).

- Constructor `PriorityQueue(o=default_ordering(), pairs::Pairs{K,V}...)` idem with a
  dictionary.

- Deal with non 1-based indices in binary heaps and offset arrays in fast priority queues.

- Remove unused `heapify_down!`, `heapify_up!`, and `require_one_based_indexing`.

- Change `show` method for `AbstractPriorityQueue` to reflect that.

- Implement `merge`, `merge!`, and `reverse`:
  ```julia
  reverse(h::BinaryHeap) = BinaryHeap(copy(values(h), 1:lenght(h)), reverse(ordering(h)))
  reverse(pq::PriorityQueue{K,V,O,T}) where {K,V,O,T} =
      merge!(PriorityQueue{K,V}(reverse(ordering(pq)), T), pq)
  reverse(pq::FastPriorityQueue{V,N,O,T}) where {V,N,O,T} =
      merge!(FastPriorityQueue{V,N}(reverse(ordering(pq)), T), size(index(pq)); pq)
  ...
  ```
