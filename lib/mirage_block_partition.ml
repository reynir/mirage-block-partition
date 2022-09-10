module type PARTITION_AT = sig
  (* XXX: should this be in bytes? in sectors?? *)
  val first_length : int64
end

module Make(B : Mirage_block.S)(P : PARTITION_AT) = struct
  type t = {
    b : B.t;
    info : Mirage_block.info;
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
    Lwt.return { b.info with size_sectors }

  let is_within b sector_start buffers =
    let buffers_len =
      buffers
      |> List.fold_left (fun acc cs -> acc + Cstruct.length cs) 0
      |> Int64.of_int
    in
    let num_sectors =
      Int64.(add 1L (div
                       (add buffers_len (pred (of_int b.info.sector_size)))
                       (of_int b.info.sector_size)))
    in
    let sector_end = Int64.add sector_start num_sectors in
    let sector_start = Int64.add sector_start b.sector_start in
    sector_start >= b.sector_start && sector_end < b.sector_end

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

  let connect b =
    let ( let* ) = Lwt.bind in
    let* info = B.get_info b in
    let sector_start = 0L
    and sector_end = info.size_sectors in
    let sector_mid, misalignment =
      Int64.(div P.first_length (of_int info.sector_size),
             rem P.first_length (of_int info.sector_size))
    in
    if misalignment <> 0L then
      Lwt.return (Error (`Bad_partition
                           ("Partition must be aligned with sector size " ^
                            string_of_int info.sector_size ^ " (" ^
                            Int64.to_string misalignment ^ " bytes misaligned)")))
    else if sector_mid < sector_start || sector_mid > sector_end then
      Lwt.return (Error (`Bad_partition "Illegal partition point"))
    else
      Lwt.return
        (Ok ({ b; info; sector_start; sector_end = sector_mid },
             { b; info; sector_start = sector_mid; sector_end }))

  let disconnect b =
    (* XXX disconnect both?! *)
    B.disconnect b.b
end
