module Make(B : Mirage_block.S) : sig
  include Mirage_block.S

  val connect : int64 -> B.t -> (t * t, [> `Bad_partition ]) result Lwt.t
  (** [connect first_sectors b] returns two block device partitions of b with
      the first partition of [first_sectors] sectors, and the second partition
      the remaining space, if any.
  *)

  val subpartition : int64 -> t -> (t * t, [> `Bad_partition ]) result
  (** [subpartition first_sectors b] further partitions a partition into two sub
      partitions.
  *)

  val get_offset : t -> int64
  (** [get_offset b] is the sector offset of the partition relative to the
      underlying block device. *)
end
