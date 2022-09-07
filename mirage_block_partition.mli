module type PARTITION_AT = sig
  (* XXX: should this be in bytes? in sectors?? *)
  val partition_at : int64
end

module Make(B : Mirage_block)(P : PARTITION_AT) : sig
  include Mirage_block.S
  val connect : B.t -> (t * t, error) result Lwt.t
end
