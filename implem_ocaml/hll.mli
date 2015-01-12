
module type Params =
sig
  val nlz : int -> int
  val p : int
  val hash_size : int
end

module Make : functor (P : Params) -> sig

  (** add hashed element to the functor instance global variable (side effect) *)
  val add_item : int -> unit

  (** count the number of different items previously added hyperloglog *)
  val count : unit -> float
end
