## mirage-block-partition -- block device partitioning

Mirage-block-partition lets you view a mirage block device as smaller partitions.
A partition can be further subpartitioned into smaller partitions.

```OCaml
module Make(B : Mirage_block.S)(Clock : Mirage_clock.PCLOCK) = struct

  module Partitioned = Mirage_block_partition.Make(B)
  module Tar = Tar_mirage.Make_KV_RO(Partitioned)
  module Chamelon = Kv.Make(Partitioned)(Clock)

  let start b =
    let open Lwt.Syntax in
    (* b1 is the first twenty sectors, b2 is the next 8k sectors (4 MiB),
       and b3 is the remaining space. Note that the initial [connect] call is
       asynchronous while the later [subpartition] calls are not. If the
       partition point is outside the block device or subpartition then an
       exception is raised. *)
    let* b = Partitioned.connect b in
    let* { size_sectors; _ } = Partition.get_info b in
    let b1 = Partitioned.subpartition ~start:0L ~len:20L b in
    (* We can further subpartition a partition: *)
    let rest = Partition.subpartition ~start:20L ~len:(Int64.sub size_sectors 20L) in
    (* And the size of the partition will be reported correctly. *)
    let* { size_sectors = remaining; _ } = Partitioned.get_info rest in
    (* [b2] is right after [b1]. *)
    let b2 = Partitioned.subpartition ~start:0L ~len:8192L rest
    and b3 = Partitioned.subpartition ~start:8192L ~len:(Int64.sub remaining 8192L) rest in
    (* now use e.g. b1 as a tar KV store, b2 as a chamelon filesystem,
       b3 as a raw block device... *)
    let* tar = Tar.connect b1
    and* chamelon = Chamelon.connect ~program_size:16 b2 in
    ...

end
```

### mirage-block-partition-mbr

This module reads a disk labeled with a Master Boot Record and returns a list of pairs of `Mbr.Partition.t` and a block device representing the partition according to the MBR.
