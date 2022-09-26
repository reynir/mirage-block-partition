module Make(B : Mirage_block.S) : sig
  include Mirage_block.S
  val connect : B.t -> ((Mbr.Partition.t * t) list, [ `Overlapping_partitions ]) result Lwt.t
end
