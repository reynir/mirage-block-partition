## v0.2.0

- BREAKING disconnect no longer disconnects the underlying block device and
  instead marks the partition as disconnected. This does not propagate to
  subpartitions (#9 @dianaoigo)
- BREAKING `connect` and `subpartition` no longer return pairs. Instead,
  `connect` returns a partition representing the whole block device.
  `subpartition` then takes labelled arguments `~start` and `~len`. While the
  "split" semantics has some nice properties it does not fit very well with how
  partitions are specified in partition tables (#13 @reynir)
- mirage-block-partition-mbr is simplified and no longer checks for overlapping
  partitions. The module may be removed or revised in a future release.

## v0.1.0

- Initial release
