module Make(B : Mirage_block.S) = struct
  type t = {
    b : B.t;
    sector_size : int;
    (* inclusive *)
    sector_start : int64;
    (* exclusive *)
    sector_end : int64;
    mutable connected : bool;
  }

  type nonrec error = [ 
    | Mirage_block.error
    | `Block of B.error
    | `Out_of_bounds ]
  type nonrec write_error = [
    | Mirage_block.write_error
    | `Block of B.write_error
    | `Out_of_bounds ]

  let pp_error ppf = function
    | `Block e | (#Mirage_block.error as e) -> B.pp_error ppf e
    | `Out_of_bounds -> Fmt.pf ppf "Operation out of partition bounds"

  let pp_write_error ppf = function
    | `Block e | (#Mirage_block.write_error as e) -> B.pp_write_error ppf e
    | `Out_of_bounds -> Fmt.pf ppf "Operation out of partition bounds"

  let get_info b =
    let size_sectors = Int64.(sub b.sector_end b.sector_start) in
    Lwt.map (fun info -> { info with Mirage_block.size_sectors })
      (B.get_info b.b)

  let get_offset { sector_start; _ } = sector_start

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
    (* XXX: here and in [write] we rely on the underlying block device to check
       for alignment issues of [buffers]. *)
    if not (is_within b sector_start buffers) then
      Lwt.return (Error `Out_of_bounds)
    else if not b.connected then
      Lwt.return (Error `Disconnected)
    else
      B.read b.b (Int64.add b.sector_start sector_start) buffers
      |> Lwt_result.map_error (fun b -> `Block b)

  let write b sector_start buffers =
    if not (is_within b sector_start buffers) then
      Lwt.return (Error `Out_of_bounds)
    else if not b.connected then
      Lwt.return (Error `Disconnected)
    else
      B.write b.b (Int64.add b.sector_start sector_start) buffers
      |> Lwt_result.map_error (fun b -> `Block b)

  let connect b =
    let open Lwt.Syntax in
    let+ { Mirage_block.sector_size; size_sectors = sector_end; _ } = B.get_info b in
    { b; sector_size; sector_start = 0L; sector_end; connected = true }

  let subpartition ~start ~len t =
    let ( let* ) = Result.bind in
    if start < 0L || len < 0L then
      invalid_arg "Mirage_block_partition.subpartition'";
    let sector_start = Int64.add t.sector_start start in
    let sector_end = Int64.add sector_start len in
    let* () =
      if sector_start >= t.sector_end || sector_end > t.sector_end then
        Error `Out_of_bounds
      else Ok ()
    in
    let* () =
      if not t.connected then
        Error `Disconnected
      else Ok ()
    in
    Ok { t with sector_start; sector_end }

  let disconnect b =
    if b.connected then
      b.connected <- false;
    Lwt.return_unit
end
