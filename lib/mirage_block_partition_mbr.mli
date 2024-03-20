module Make(B : Mirage_block.S) : sig
  include Mirage_block.S
  val connect : B.t -> ((Mbr.Partition.t * t) list, error) result Lwt.t
  (** [connect b] returns a pair of [Mbr.Partition.t] from the MBR and a
      corresponding device representing the partition. An error is returned if
      any of the partitions are outside the device.
      @raise Invalid_argument if the the sector size of [b] is not exactly 512 bytes.
      @raise Failure if reading the MBR fails or does not parse. *)
end
