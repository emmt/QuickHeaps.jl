# User visible changes in `QuickHeaps`

## Unreleased

This new version uses better [*total order*](https://en.wikipedia.org/wiki/Total_order) by
default and replaces nodes by pairs. As a consequence, there are a few breaking changes but
these should only weakly matter for the end-user.

### Breaking changes

- `Node{K,V}` and `AbstractNode{K,V}` have been suppressed. Nodes of type
  `AbstractNode{K,V}` are replaced by pairs of type `Pair{K,V}`. As a result, the node type
  is no longer an optional argument in priority queues constructors and `dequeue_node!` has
  been suppressed (call `dequeue_pair!` instead).

- Default ordering for binary heaps and priority queues is always `TotalMin` instead of
  `Base.Order.Forward` or `FastMin` (depending on the kind of the ordered structure). With
  `TotalMin`, values are sorted in increasing order followed by `NaN` then `missing` values.
  Compared to `FastMin`, `TotalMin` is barely slower (on floating-point values) but
  implements a [*total min order*](https://en.wikipedia.org/wiki/Total_order) while
  `FastMin` leaves the order of `NaN` values undefined and fails on `missing` values.
  `TotalMin` yields the same order as `Base.Order.Forward` which is the default order in
  Julia sorting algorithms. On floating-point values, `TotalMin` is faster by almost a
  factor of 2 than `Base.Order.Forward`.

- `SafeMin` and `SafeMax` are no longer available. They were respectively aliases to
  `Base.Order.Forward` and `Base.Order.Reverse`. `TotalMin` is equivalent to but faster than
  `Base.Order.Forward`.

- Aliases `FastForward`, `FastReverse` and non-exported public symbol
  `QuickHeaps.FastForwardOrdering` are no longer available. `FastForward` and `FastReverse`
  are respectively equivalent to `FastMin` and `FastMax` but are not recommended, for
  reasons given above.

- `Base.Order.Ordering`, `Base.Order.ForwardOrdering`, `Base.Order.ReverseOrdering`,
  `Base.Order.Forward`, and `Base.Order.Reverse` are no longer exported by `QuickHeaps`.

- Deprecated and non-exported method `QuickHeaps.to_eltype(A, x)` has been suppressed, it
  was equivalent to `as(eltype(A), x)` using the
  [`TypeUtils`](https://github.com/emmt/TypeUtils.jl) package.

- Documented but non-exported functions `QuickHeaps.index`,
  `QuickHeaps.is_one_based_unit_range`, `QuickHeaps.nodes`, `QuickHeaps.ordering`, and
  `QuickHeaps.typename`, have been suppressed. Call `Base.Order.Ordering(A)` to query the
  order in binary heap or priority queue `A`.

- `getindex` throws `KeyError` (was `ArgumentError`) for non-existing keys in priority
  queues.

### Added

- Pass all tests with [`Aqua.jl`](https://github.com/JuliaTesting/Aqua.jl).

- New `TotalMin` and `TotalMax` orders implement a [*total
  order*](https://en.wikipedia.org/wiki/Total_order) where values are respectively sorted in
  increasing and decreasing order followed by `NaN` then `missing` values.

- Fast priority queues of type `FastPriorityQueue` can be directly indexed as
  multidimensional arrays.

### Changed

- Use the `@public` macro of the [`TypeUtils`](https://github.com/emmt/TypeUtils.jl) package
  for non-exported public methods.

- Syntax `peek(T::Type, pq::AbstractPriorityQueue)` is deprecated in favor of `peek(pq,T)`
  to specify the type `T` of the returned object.

- The default ordering is `QuickHeaps.TotalMin` for all structures. It behaves like `isless`
  which implements the default ordered of most sorting algorithms in Julia: NaN's are
  considered as being greater than any other floating-point value, and `missing` to be
  greater than anything else. However, by using non-branching bitwise operators instead of
  logical operators, `QuickHeaps.TotalMin` is much faster than `isless` while being nearly as
  fast as `<`.

- Methods `append!` and `prepend` are purposely not supported by binary heaps.

### Fixed

- Ambiguities have been fixed.

- Default `TotalMin` order fixes the behavior of `FastMin` (the former default for
  `FastBinaryHeaps`) which leaves undefined the order of `NaN` and `missing` values. This
  new default implements the same order as `Base.Order.Forward` (the former default for
  other ordered structures) but is about twice faster.

### Removed

- `AbstractNode` and `Node` types and related API have been suppressed.

- Many non-exported but documented functions have been suppressed: `storage`, `ordering`,
  `index`, `in_range`, `nodes`.


## Version 0.2.3 (2025-09-17)

- Fix compatibility with `DataStructures`.

## Version 0.2.2 (2024-05-15)

- Update compatibility for `TypeUtils`.

## Version 0.2.1 (2023-07-14)

- Package `TypeUtils` replaces `ArrayTools`.

- Non-exported method `QuickHeaps.to_eltype(A, x)` has been deprecated, use `as(eltype(A),
  x)` instead.

## Version 0.2.0 (2023-02-25)

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

## Version 0.1.2 (2022-09-26)

- Fix a few bugs.
- provide docs.
- Extend tests.
