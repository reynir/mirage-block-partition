## mirage-block-partition -- naïve block device partitioning

Mirage-block-partition lets you view a mirage block device as smaller partitions.



```OCaml
module Make(B : Mirage_block.S)(Clock : Mirage_clock.PCLOCK) = struct

  module Partitioned = Mirage_block_partition.Make(B)
  module Tar = Tar_mirage.Make_KV_RO(Partitioned)
  module Chamelon = Kv.Make(Partitioned)(Clock)

  let start b =
    let open Lwt.Syntax in
    let* r =
      let open Lwt_result.Syntax in
      (* b1 is the first kilobyte, b2 is the next 4 MB,
         and b3 is the remaining space *)
      let* b1, rest = Partitioned.connect 1024 in
      let+ b2, b3 = Partitioned.subpartition 0x400000
      b1, b2, b3
    in
    match r with
    | Error e ->
      Lwt.fail_with (Fmt.str "partition error: %a" Partitioned.pp_error e)
    | Ok (b1, b2, b3) ->
      (* now use e.g. b1 as a tar KV store, b2 as a chamelon filesystem,
         b3 as a raw block device... *)
      let* tar = Tar.connect b1
      and* chamelon = Chamelon.connect ~program_size:16 b2 in
      ...

end
```
