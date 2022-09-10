open Lwt.Syntax

module P = struct
  let first_length = 512L
end
module Partitioned = Mirage_block_partition.Make(Mirage_block_mem)(P)

let setup f () =
  let* b = Mirage_block_mem.connect "test" in
  let* r = Partitioned.connect b in
  let b1, b2 =
    match r with
    | Ok (b1, b2) -> b1, b2
    | Error e -> Alcotest.failf "Partitioned.connect failed: %a" Partitioned.pp_error e
  in
  let* _ = f (b, b1, b2) in
  let* () = Partitioned.disconnect b2 in
  let* () = Partitioned.disconnect b1 in
  Mirage_block_mem.disconnect b

let sector_size (b, b1, b2) =
  let+ info = Mirage_block_mem.get_info b
  and+ info1 = Partitioned.get_info b1
  and+ info2 = Partitioned.get_info b2 in
  Alcotest.(check int "partition1" info.sector_size info1.sector_size);
  Alcotest.(check int "partition2" info.sector_size info2.sector_size)

let first_partition_size (b, b1, b2) =
  let+ info = Mirage_block_mem.get_info b
  and+ info1 = Partitioned.get_info b1
  and+ _info2 = Partitioned.get_info b2 in
  Alcotest.(check int64 "sector_size" P.first_length
              Int64.(mul info1.size_sectors (of_int info.sector_size)))

let simple =
  "Simple tests", [
    "Print infos", `Quick, setup sector_size;
    "First partition size", `Quick, setup first_partition_size;
  ]

let () =
  Lwt_main.run @@
  Alcotest_lwt.run "Mirage_block_partition"
    [ simple ]
