## mirage-block-partition -- block device partitioning

Mirage-block-partition lets you view a mirage block device as smaller partitions.
The interface is structured around (successively) partitioning into two smaller partitions.
The reason for this at times perhaps slightly inconvenient interface is it makes it harder to accidentally construct overlapping partitions.

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
    let* b1, rest = Partitioned.connect 20L b in
    let b2, b3 = Partitioned.subpartition 8192L rest in
    (* now use e.g. b1 as a tar KV store, b2 as a chamelon filesystem,
       b3 as a raw block device... *)
    let* tar = Tar.connect b1
    and* chamelon = Chamelon.connect ~program_size:16 b2 in
    ...

end
```

### mirage-block-partition-mbr

This module reads a disk labeled with a Master Boot Record and returns a list of pairs of `Mbr.Partition.t` and a block device representing the partition according to the MBR.
