module Make(B : Mirage_block.S) = struct
  module P = Mirage_block_partition.Make(B)
  include P

  type section =
    | Unused of int64
    | Partition of Mbr.Partition.t * int64

  let sections offset partitions =
    List.fold_left
      (fun r p ->
         let ( let* ) = Result.bind in
         let* offset, ps = r in
         let sector_start = Mbr.Partition.sector_start p
         and size_sectors = Mbr.Partition.size_sectors p in
         let* () =
           if sector_start < offset then
             Error `Overlapping_partitions
           else Ok ()
         in
         let p =
           Partition (p, size_sectors)
         in
         let ps =
           if sector_start > offset then
             p :: Unused (Int64.sub offset sector_start) :: ps
           else
             p :: ps
         in
         Ok (Int64.add sector_start size_sectors, ps))
      (Ok (offset, []))
      partitions
    |> Result.map snd
    |> Result.map List.rev


  let subpartition b (mbr : Mbr.t) =
    let partitions =
      List.sort (fun p1 p2 ->
          Int32.unsigned_compare
            p1.Mbr.Partition.first_absolute_sector_lba
            p2.Mbr.Partition.first_absolute_sector_lba)
        mbr.partitions
    in
    match sections (P.get_offset b) partitions with
    | Error _ as e -> e
    | Ok partitioning ->
      List.fold_left
        (fun acc p ->
           let ( let* ) = Result.bind in
           let* rest, ps = acc in
           match p with
           | Unused length ->
             let _, rest = P.subpartition length rest in
             Ok (rest, ps)
           | Partition (p, length) ->
             let b, rest = P.subpartition length rest in
             Ok (rest, (p, b) :: ps))
        (Ok (b, []))
        partitioning
      |> Result.map snd
      |> Result.map List.rev

  let connect b =
    let open Lwt.Syntax in
    let* { Mirage_block.sector_size; _ } = B.get_info b in
    (* XXX: MBRs *usually* expect a sector size of 512 bytes, and a lot of
       software will not behave correctly if sector size != 512. *)
    if sector_size <> Mbr.sizeof then
      Printf.ksprintf invalid_arg "Bad sector size: %d" sector_size;
    let* (mbr, rest) = P.connect 1L b in
    let buf = Cstruct.create Mbr.sizeof in
    let* r = P.read mbr 0L [buf] in
    begin match r with
      | Error e -> Format.kasprintf failwith "MBR read error: %a" pp_error e
      | Ok () -> ()
    end;
    match Mbr.unmarshal buf with
    | Error e -> Printf.ksprintf Lwt.fail_with "Bad MBR: %s" e
    | Ok mbr ->
      Lwt.return (subpartition rest mbr)
end

