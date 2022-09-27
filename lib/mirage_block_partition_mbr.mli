module Make(B : Mirage_block.S) : sig
  include Mirage_block.S
  val connect : B.t -> ((Mbr.Partition.t * t) list, [ `Overlapping_partitions ]) result Lwt.t
  (** [connect b] returns a pair of [Mbr.Partition.t] from the MBR and a
      corresponding device representing the partition. An error is returned if
      any of the partitions overlap.
      @raise Invalid_argument if the the sector size of [b] is not exactly 512 bytes or if any of the partitions are outsie the device.
      @raise Failure if reading the MBR fails or does not parse. *)
end
