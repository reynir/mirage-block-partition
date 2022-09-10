open Lwt.Syntax

module P = struct
  let first_length = 512L
end
module Partitioned = Mirage_block_partition.Make(Mirage_block_mem)(P)

let cstruct = Alcotest.testable Cstruct.hexdump_pp Cstruct.equal
(* XXX: polymorphic compare :'( *)
let partitioned_error = Alcotest.testable Partitioned.pp_error (=)
let partitioned_write_error = Alcotest.testable Partitioned.pp_write_error (=)

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

let write_all_partitioned b v =
  let* info = Partitioned.get_info b in
  let buf = Cstruct.create info.Mirage_block.sector_size in
  Cstruct.memset buf v;
  Fmt.pr "Writing %Ld sector(s) of size %d\n" info.size_sectors info.sector_size;
  Partitioned.write b 0L
    (List.init (Int64.to_int info.size_sectors) (Fun.const buf))


let sector_size (b, b1, b2) =
  let+ info = Mirage_block_mem.get_info b
  and+ info1 = Partitioned.get_info b1
  and+ info2 = Partitioned.get_info b2 in
  Alcotest.(check int "partition1" info.sector_size info1.sector_size);
  Alcotest.(check int "partition2" info.sector_size info2.sector_size)

let total_size (b, b1, b2) =
  let+ info = Mirage_block_mem.get_info b
  and+ info1 = Partitioned.get_info b1
  and+ info2 = Partitioned.get_info b2 in
  let open Int64 in
  let b_size = mul info.size_sectors (of_int info.sector_size)
  and b1_size = mul info1.size_sectors (of_int info1.sector_size)
  and b2_size = mul info2.size_sectors (of_int info2.sector_size) in
  Alcotest.(check int64 "total_size" b_size (add b1_size b2_size))

let first_partition_size (b, b1, b2) =
  let+ info = Mirage_block_mem.get_info b
  and+ info1 = Partitioned.get_info b1
  and+ _info2 = Partitioned.get_info b2 in
  Alcotest.(check int64 "sector_size" P.first_length
              Int64.(mul info1.size_sectors (of_int info.sector_size)))

let write_first (b, b1, b2) =
  let* info = Mirage_block_mem.get_info b
  and* info1 = Partitioned.get_info b1 in
  let* r = write_all_partitioned b1 1 in
  Alcotest.(check (result unit partitioned_write_error) "write_result" (Ok ()) r);
  let buf = Cstruct.create info.sector_size
  and buf' = Cstruct.create info1.sector_size in
  let* r = Mirage_block_mem.read b 0L [buf] in
  Alcotest.(check (result unit reject) "read_mem" (Ok ()) r);
  let* r = Partitioned.read b1 0L [buf'] in
  Alcotest.(check (result unit partitioned_error) "read_p1" (Ok ()) r);
  Alcotest.(check cstruct "equal" buf buf');
  let* r = Mirage_block_mem.read b 1L [buf] in
  Alcotest.(check (result unit reject) "read_mem" (Ok ()) r);
  let+ r = Partitioned.read b2 0L [buf'] in
  Alcotest.(check (result unit partitioned_error) "read_p2" (Ok ()) r);
  Alcotest.(check cstruct "equal" buf buf')

let write_second (b, b1, b2) =
  let* info = Mirage_block_mem.get_info b
  and* info2 = Partitioned.get_info b2 in
  let* r = write_all_partitioned b2 1 in
  Alcotest.(check (result unit partitioned_write_error) "write_result" (Ok ()) r);
  let buf = Cstruct.create info.sector_size
  and buf' = Cstruct.create info2.sector_size in
  let* r = Mirage_block_mem.read b 1L [buf] in
  Alcotest.(check (result unit reject) "read_mem" (Ok ()) r);
  let* r = Partitioned.read b2 0L [buf'] in
  Alcotest.(check (result unit partitioned_error) "read_p2" (Ok ()) r);
  Alcotest.(check cstruct "equal" buf buf');
  let* r = Mirage_block_mem.read b 0L [buf] in
  Alcotest.(check (result unit reject) "read_mem" (Ok ()) r);
  let* r = Partitioned.read b1 0L [buf'] in
  Alcotest.(check (result unit partitioned_error) "read_p1" (Ok ()) r);
  Alcotest.(check cstruct "equal" buf buf');
  let* r = Mirage_block_mem.read b (Int64.pred info.size_sectors) [buf] in
  Alcotest.(check (result unit reject) "read_mem" (Ok ()) r);
  let+ r = Partitioned.read b2 (Int64.pred info2.size_sectors) [buf'] in
  Alcotest.(check (result unit partitioned_error) "read_p2" (Ok ()) r);
  Alcotest.(check cstruct "equal" buf buf')

let read_write_first_after_end (_b, b1, _b2) =
  let* info1 = Partitioned.get_info b1 in
  let buf = Cstruct.create info1.sector_size in
  let* r = Partitioned.read b1 info1.size_sectors [buf] in
  Result.iter (fun () -> Alcotest.fail "Expected read error") r;
  let+ r = Partitioned.write b1 info1.size_sectors [buf] in
  Result.iter (fun () -> Alcotest.fail "Expected write error") r

let read_write_second_after_end (_b, _b1, b2) =
  let* info2 = Partitioned.get_info b2 in
  let buf = Cstruct.create info2.sector_size in
  let* r = Partitioned.read b2 (Int64.succ info2.size_sectors) [buf] in
  Result.iter (fun () -> Alcotest.fail "Expected read error1") r;
  let* r = Partitioned.read b2 info2.size_sectors [buf] in
  Result.iter (fun () -> Alcotest.fail "Expected read error2") r;
  let+ r = Partitioned.write b2 info2.size_sectors [buf] in
  Result.iter (fun () -> Alcotest.fail "Expected write error") r

let simple =
  "Simple tests", [
    "Sector sizes equal", `Quick, setup sector_size;
    "total size equal", `Quick, setup total_size;
    "First partition size", `Quick, setup first_partition_size;
    "Fill first partition", `Quick, setup write_first;
    "Fill second partition", `Quick, setup write_second;
    "Read/write after end in first partition", `Quick, setup read_write_first_after_end;
    "Read/write after end in second partition", `Quick, setup read_write_second_after_end;
  ]

let () =
  Lwt_main.run @@
  Alcotest_lwt.run "Mirage_block_partition"
    [ simple ]
