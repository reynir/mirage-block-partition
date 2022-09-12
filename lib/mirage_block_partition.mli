module Make(B : Mirage_block.S) : sig
  include Mirage_block.S

  val connect : int64 -> B.t -> (t * t, error) result Lwt.t
  (** [connect first_length b] returns two block device partitions of b with
   * the first partition of length [first_length] bytes, and the second
   * partition the remaining space. [first_length] must be aligned to the
   * sector size. *)

  val subpartition : int64 -> t -> (t * t, error) result
  (** [connect first_length b] further partitions a partition into two sub
   * partitions. [first_length] must be aligned to the sector size. *)
end
