- Implement `pushpop!` and `heapreplace!` (see https://en.wikipedia.org/wiki/Binary_heap).

- Add a switch to automatically cope with NaN's.

- `enqueue!(Val(:up), pq, k => v)` and `enqueue!(Val(:down), pq, k => v)`

- `force_up!` and `force_down!` needed? No, it is sufficient to not use the
  node that is deleted or replaced in `delete!` or `enqueue!`.

- Add `@inbounds` and benchmark priority queues.

- Add constructors (needs a readable way to specify the node type).

- Improve doc.
