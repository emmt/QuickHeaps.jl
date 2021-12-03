- Add `@inbounds` and benchmark priority queues.

- Add priority queue constructors (needs a readable way to specify the node
  type).

- Implement `pushpop!` and `heapreplace!` (see
  https://en.wikipedia.org/wiki/Binary_heap).

- Provide aliases `MinHeap`, `MaxHeap`, `FastMinHeap`, and `FastMaxHeap`.

- Add a switch to automatically cope with NaN's.

- `enqueue!(Val(:up), pq, k => v)` and `enqueue!(Val(:down), pq, k => v)`

- Priority queue index can be any sub-type of `AbstractDict` of `AbstractArray`.

- Improve doc.
