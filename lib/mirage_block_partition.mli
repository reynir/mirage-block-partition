module type PARTITION_AT = sig
  (* XXX: should this be in bytes? in sectors?? *)
  val first_length : int64
end

module Make(B : Mirage_block.S)(_ : PARTITION_AT) : sig
  include Mirage_block.S
  val connect : B.t -> (t * t, error) result Lwt.t
end
