## mirage-block-partition -- block device partitioning

Mirage-block-partition lets you view a mirage block device as smaller partitions.
The interface is structured around (successively) partitioning into two smaller partitions.
The reason for this at times perhaps slightly inconvenient interface is it makes it harder to accidentally construct overlapping partitions.

```OCaml
module Make(B : Mirage_block.S)(Clock : Mirage_clock.PCLOCK) = struct
  (* Create a module for partitioning a Mirage block device *)
  module Partitioned = Mirage_block_partition.Make(B)

  (* Create a module for accessing a tar KV store on a partitioned block device *)
  module Tar = Tar_mirage.Make_KV_RO(Partitioned)

  (* Create a module for accessing a Chameleon filesystem on a partitioned block device *)
  module Chameleon = Kv.Make(Partitioned)(Clock)

  (* Connect to the block device and partition it into three sub-blocks *)
  let%bind connect_and_partition b =
    let%bind b1, rest = Partitioned.connect (Sectors.of_int 20) b in
    let b2, b3 = Partitioned.subpartition (Sectors.of_int 8192) rest in
    (* Return a tuple containing the three sub-blocks *)
    return (b1, b2, b3)

  let start b =
    let open Lwt.Syntax in
    (* Connect to the block device and partition it into three sub-blocks *)
    let* b1, rest = Partitioned.connect (Sectors.of_int 20) b in
    let b2, b3 = Partitioned.subpartition (Sectors.of_int 8192) rest in

    (* Connect to the tar KV store on the first sub-block *)
    let* tar = Tar.connect b1 in

    (* Connect to the Chameleon filesystem on the second sub-block *)
    let* chameleon = Chameleon.connect ~program_size:16 b2 in

    (* Other code using the connected devices *)
    ...
end

```



### mirage-block-partition-mbr

This module reads a disk labeled with a Master Boot Record and returns a list of pairs of `Mbr.Partition.t` and a block device representing the partition according to the MBR.
