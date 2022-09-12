module Make(B : Mirage_block.S) = struct
  type t = {
    b : B.t;
    sector_size : int;
    (* inclusive *)
    sector_start : int64;
    (* exclusive *)
    sector_end : int64;
  }

  type nonrec error = [ 
    | Mirage_block.error
    | `Block of B.error
    | `Out_of_bounds
    | `Bad_partition of string ]
  type nonrec write_error = [
    | Mirage_block.write_error
    | `Block of B.write_error
    | `Out_of_bounds ]

  let pp_error ppf = function
    | `Block e | (#Mirage_block.error as e) -> B.pp_error ppf e
    | `Out_of_bounds -> Fmt.pf ppf "Operation out of partition bounds"
    | `Bad_partition e -> Fmt.pf ppf "Bad partition: %s" e

  let pp_write_error ppf = function
    | `Block e | (#Mirage_block.write_error as e) -> B.pp_write_error ppf e
    | `Out_of_bounds -> Fmt.pf ppf "Operation out of partition bounds"

  let get_info b =
    let size_sectors = Int64.(sub b.sector_end b.sector_start) in
    Lwt.map (fun info -> { info with Mirage_block.size_sectors })
      (B.get_info b.b)

  let is_within b sector_start buffers =
    let buffers_len =
      List.fold_left (fun acc cs -> Int64.(add acc (of_int (Cstruct.length cs))))
        0L buffers
    in
    let num_sectors =
      let sector_size = Int64.of_int b.sector_size in
      Int64.(div (add buffers_len (pred sector_size))
               sector_size)
    in
    let sector_start = Int64.add sector_start b.sector_start in
    let sector_end = Int64.add sector_start num_sectors in
    sector_start >= b.sector_start && sector_end <= b.sector_end

  let read b sector_start buffers =
    if is_within b sector_start buffers
    then
      B.read b.b (Int64.add b.sector_start sector_start) buffers
      |> Lwt_result.map_error (fun b -> `Block b)
    else
      Lwt.return (Error `Out_of_bounds)

  let write b sector_start buffers =
    if is_within b sector_start buffers
    then
      B.write b.b (Int64.add b.sector_start sector_start) buffers
      |> Lwt_result.map_error (fun b -> `Block b)
    else
      Lwt.return (Error `Out_of_bounds)

  let partition b ~sector_size ~sector_start ~sector_end ~first_length =
    let sector_mid, misalignment =
      Int64.(div first_length (of_int sector_size),
             rem first_length (of_int sector_size))
    in
    if misalignment <> 0L then
      Error (`Bad_partition
               ("Partition must be aligned with sector size " ^
                string_of_int sector_size ^ " (" ^
                Int64.to_string misalignment ^ " bytes misaligned)"))
    else if sector_mid < sector_start || sector_mid > sector_end then
      Error (`Bad_partition "Illegal partition point")
    else
      Ok ({ b; sector_size; sector_start; sector_end = sector_mid },
          { b; sector_size; sector_start = sector_mid; sector_end })

  let connect first_length b =
    let open Lwt.Syntax in
    let+ { Mirage_block.sector_size; size_sectors = sector_end; _ } = B.get_info b in
    let sector_start = 0L in
    partition b ~sector_size ~sector_start ~sector_end ~first_length

  let subpartition first_length { b; sector_size; sector_start; sector_end } =
    partition b ~sector_size ~sector_start ~sector_end ~first_length

  let disconnect b =
    (* XXX disconnect both?! *)
    B.disconnect b.b
end
