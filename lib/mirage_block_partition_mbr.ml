module Make(B : Mirage_block.S) = struct
  module P = Mirage_block_partition.Make(B)
  include P

  type partition =
    | Empty of int64
    | Partition of Mbr.Partition.t * int64

  let partitioning partitions =
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
      (Ok (1L, []))
      partitions
    |> Result.map snd
    |> Result.map List.rev


  let connect b (mbr : Mbr.t) =
    let open Lwt.Infix in
    let partitions =
      List.sort (fun p1 p2 ->
          Int32.unsigned_compare
            p1.Mbr.Partition.first_absolute_sector_lba
            p2.Mbr.Partition.first_absolute_sector_lba)
        mbr.partitions
    in
    match partitioning partitions with
    | Error _ as e -> Lwt.return e
    | Ok partitioning ->
      P.connect 1L b >>= fun r ->
      match r with
      | Error _ as e -> Lwt.return e
      | Ok (_, rest) ->
        let r =
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
            (Ok (rest, []))
            partitioning
          |> Result.map snd
          |> Result.map List.rev
        in
        Lwt.return r
end

