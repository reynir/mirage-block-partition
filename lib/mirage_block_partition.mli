module Make(B : Mirage_block.S) : sig
  include Mirage_block.S

(**  val connect : int64 -> B.t -> (t * t) Lwt.t
  (** [connect first_sectors b] returns two block device partitions of b with
      the first partition of [first_sectors] sectors, and the second partition
      the remaining space, if any.
      @raises Invalid_argument if the partition point is outside the device.
  *) *)

  module type CONNECT = sig
  type 'a io
  type page_aligned_buffer

  val connect :
    ?name:string ->
    (Part.t * page_aligned_buffer) list ->
    'a io ->
    (page_aligned_buffer * Part.t) list Lwt.t

  val connect' :
    ?name:string ->
    offset:int64 ->
    size:int64 ->
    'a io ->
    (page_aligned_buffer * Part.t) Lwt.t
    end

  val subpartition : int64 -> t -> (t * t)
  (** [subpartition first_sectors b] further partitions a partition into two sub
      partitions.
      @raises Invalid_argument if the partition point is outside the partition.
  *)

  val get_offset : t -> int64
  (** [get_offset b] is the sector offset of the partition relative to the
      underlying block device. *)
end


