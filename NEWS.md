# User visible changes in `QuickHeaps`

## Unreleased

### Added

- Pass all tests with [`Aqua.jl`](https://github.com/JuliaTesting/Aqua.jl).

### Changed

- Use the `@public` macro of the [`TypeUtils`](https://github.com/emmt/TypeUtils.jl) package
  for non-exported public methods.

- Deprecated and non-exported method `QuickHeaps.to_eltype(A, x)` has been suppressed, it
  was equivalent to `as(eltype(A), x)` using the
  [`TypeUtils`](https://github.com/emmt/TypeUtils.jl) package.

### Fixed

- Ambiguities have been fixed.

## Version 0.2.2

- Update compatibility for `TypeUtils`.

## Version 0.2.1

- Package `TypeUtils` replaces `ArrayTools`.

- Non-exported method `QuickHeaps.to_eltype(A, x)` has been deprecated, use `as(eltype(A),
  x)` instead.

## Version 0.2.0

This version mainly provides a cleaner API where priority queues behave more like
dictionaries.

- Provide `dequeue_node!` which removes the root node from a priority queue and returns it.
  This is similar to `dequeue_pair!` or `pop!` which both return a `Pair`. The syntax
  `dequeue(T,pq)!` to remove the root node from `pq` and return it converted to type `T` is
  no longer supported; call `T(dequeue_node!(pq))` or `convert(T, dequeue_node!(pq))`
  instead.

- The noun *node* is replaced by *value* for a binary-heap. The non-exported method
  `QuickHeaps.nodes(h)` has been renamed as `QuickHeaps.storage(h)` to retrieve the object
  backing the storage of the binary heap `h`.

- Change parameters of priority queue types which are now `AbstractPriorityQueue{K,V,O}`,
  `PriorityQueue{K,V,O,T}`, and `FastPriorityQueue{V,N,O,T}` with `K` the type of the keys,
  `V` the type of the priority values, `O` the type of the ordering, `T<:AbstractNode{K,V}`
  the type of the nodes, and `N` the number of dimensions.

- Priority queues behave more like dictionaries. Methods `get(pq,key,def)` and
  `getkey(pq,key,def)` yield the value (was the node) and the key associated with `key` in
  priority queue `pq` or `def` if such a key does not exist.

- `heapify_down!` and `heapify_up!` return the array.

- Remove unused non-exported methods `unsafe_heapify!`, `require_one_based_indexing`, ...


## Version 0.1.2

- Fix a few bugs.
- provide docs.
- Extend tests.
