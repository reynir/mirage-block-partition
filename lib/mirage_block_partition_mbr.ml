module Make (B : Mirage_block.S) = struct
  module P = Mirage_block_partition.Make (B)
  include P

  let subpartition b (mbr : Mbr.t) =
    let ( let* ) = Result.bind in
    let* partitions =
      List.fold_left
        (fun acc p ->
          let* acc = acc in
          let start = Mbr.Partition.sector_start p in
          let len = Mbr.Partition.size_sectors p in
          let* p' = P.subpartition ~start ~len b in
          Ok ((p, p') :: acc))
        (Ok []) mbr.partitions
    in
    Ok (List.rev partitions)

  let connect b =
    let open Lwt.Syntax in
    let* { Mirage_block.sector_size; _ } = B.get_info b in
    (* XXX: MBRs *usually* expect a sector size of 512 bytes, and a lot of
       software will not behave correctly if sector size != 512. *)
    if sector_size <> Mbr.sizeof then
      Printf.ksprintf invalid_arg "Bad sector size: %d" sector_size;
    let* b = P.connect b in
    let buf = Cstruct.create Mbr.sizeof in
    let* r = P.read b 0L [ buf ] in
    (match r with
    | Error e -> Format.kasprintf failwith "MBR read error: %a" pp_error e
    | Ok () -> ());
    match Mbr.unmarshal buf with
    | Error e -> Printf.ksprintf Lwt.fail_with "Bad MBR: %s" e
    | Ok mbr -> Lwt.return (subpartition b mbr)
end
