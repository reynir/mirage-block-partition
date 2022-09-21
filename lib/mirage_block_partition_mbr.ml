module Make(B : Mirage_block.S) = struct
  module P = Mirage_block_partition.Make(B)
  include P

  type connect_error = [
    | `Bad_partition
    | `Overlapping_partitions
    | `Mbr_read of P.error
    | `Bad_mbr of string
    | `Sector_size of int
  ]

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
             let* _, rest = P.subpartition length rest in
             Ok (rest, ps)
           | Partition (p, length) ->
             let* b, rest = P.subpartition length rest in
             Ok (rest, (p, b) :: ps))
        (Ok (b, []))
        partitioning
      |> Result.map snd
      |> Result.map List.rev

  let connect b : (_, connect_error) result Lwt.t =
    let open Lwt.Infix in
    let open Lwt_result.Syntax in
    B.get_info b >>= fun { Mirage_block.sector_size; _ } ->
    (* XXX: MBRs *usually* expect a sector size of 512 bytes, and a lot of
       software will not behave correctly if sector size != 512. *)
    let* () =
      if sector_size <> Mbr.sizeof then
        Lwt_result.fail (`Sector_size sector_size)
      else
        Lwt_result.return ()
    in
    let* (mbr, rest) = P.connect 1L b in
    let buf = Cstruct.create Mbr.sizeof in
    let* () = P.read mbr 0L [buf] |> Lwt_result.map_error (fun e -> `Mbr_read e) in
    match Mbr.unmarshal buf with
    | Error e -> Lwt_result.fail (`Bad_mbr e)
    | Ok mbr ->
      Lwt.return (subpartition rest mbr)

  let pp_connect_error ppf = function
    | `Bad_partition -> Fmt.pf ppf "Bad partition point"
    | `Overlapping_partitions -> Fmt.pf ppf "Partitions overlap"
    | `Mbr_read e -> Fmt.pf ppf "Error reading MBR: %a" P.pp_error e
    | `Bad_mbr e -> Fmt.pf ppf "Bad MBR: %s" e
    | `Sector_size s -> Fmt.pf ppf "Bad sector size: %d != 512" s
end

