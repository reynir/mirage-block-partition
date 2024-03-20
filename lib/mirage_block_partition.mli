module Make(B : Mirage_block.S) : sig
  include Mirage_block.S with
    type error = [ Mirage_block.error | `Block of B.error | `Out_of_bounds ]
    and type write_error = [ Mirage_block.write_error | `Block of B.write_error | `Out_of_bounds ]

  val connect : B.t -> t Lwt.t
  (** [connect b] is the partition spanning the entire block device [b]. *)

  val subpartition : start:int64 -> len:int64 -> t -> (t, error) result
  (** [subpartition ~start ~len t] is [Ok t'] where [t'] is the sub partition
      of [t] starting at [start] relative to [t] with [len] sectors, or [Error
      `Out_of_bounds] if subpartition is outside [t].

      @raise Invalid_argument when [start] or [len] are negative *)

  val get_offset : t -> int64
  (** [get_offset b] is the sector offset of the partition relative to the
      underlying block device. *)
end
