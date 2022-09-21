module Make(B : Mirage_block.S) : sig
  include Mirage_block.S
  type connect_error = private [> `Bad_partition | `Overlapping_partitions | `Sector_size of int ]
  val pp_connect_error : connect_error Fmt.t
  val connect : B.t -> ((Mbr.Partition.t * t) list, connect_error) result Lwt.t
end
