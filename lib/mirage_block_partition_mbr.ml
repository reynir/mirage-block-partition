module Make(B : Mirage_block.S) = struct
  module P = Mirage_block_partition.Make(B)
  include P

  type connect_error = [
    | `Bad_partition of string
    | `Overlapping_partitions
    | `Mbr_read of P.error
    | `Bad_mbr of string
  ]

  type section =
    | Empty of int64
    | Partition of Mbr.Partition.t * int64

  let sections offset partitions : (_, [> connect_error]) result =
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
             p :: Empty (Int64.sub offset sector_start) :: ps
           else
             p :: ps
         in
         Ok (Int64.add sector_start size_sectors, ps))
      (Ok (offset, []))
      partitions
    |> Result.map snd
    |> Result.map List.rev


  let subpartition b offset (mbr : Mbr.t) : (_, [> connect_error]) result =
    let partitions =
      List.sort (fun p1 p2 ->
          Int32.unsigned_compare
            p1.Mbr.Partition.first_absolute_sector_lba
            p2.Mbr.Partition.first_absolute_sector_lba)
        mbr.partitions
    in
    match sections offset partitions with
    | Error _ as e -> e
    | Ok partitioning ->
      List.fold_left
        (fun acc p ->
           let ( let* ) = Result.bind in
           let* rest, ps = acc in
           match p with
           | Empty length ->
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
    B.get_info b >>= fun { Mirage_block.sector_size; _ } ->
    (* XXX: MBRs *usually* expect a sector size of 512 bytes, and a lot of
       software will not behave correctly if sector size != 512. Should we
       error out?! *)
    let mbr_sectors = Int64.of_int ((Mbr.sizeof + sector_size - 1) / sector_size) in
    P.connect mbr_sectors b >>= fun r ->
    match r with
    | Error _ as e -> Lwt.return e
    | Ok (mbr, rest) ->
      let buf = Cstruct.create (sector_size * Int64.to_int mbr_sectors) in
      P.read mbr 0L [buf] >>= fun r ->
      match r with
      | Error e -> Lwt_result.fail (`Mbr_read e)
      | Ok () ->
        match Mbr.unmarshal buf with
        | Error e -> Lwt_result.fail (`Bad_mbr e)
        | Ok mbr ->
          Lwt.return (subpartition rest mbr_sectors mbr)

  let pp_connect_error ppf = function
    | `Bad_partition m -> Fmt.pf ppf "Bad partition: %s" m
    | `Overlapping_partitions -> Fmt.pf ppf "Partitions overlap"
    | `Mbr_read e -> Fmt.pf ppf "Error reading MBR: %a" P.pp_error e
    | `Bad_mbr e -> Fmt.pf ppf "Bad MBR: %s" e
end

