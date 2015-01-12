



module type Params =
sig
  val nlz : int -> int
  val p : int
  val hash_size : int
end

module Make = functor (P: Params) -> struct

  open P

  let nlz = Misc.nlz
  let p = 15
  let hash_size = 30


  let word_size = Sys.word_size

  let m = 1 lsl p
  let a = Array.make m 0

  let alpha_m = match m with
    | 16 -> 0.673
    | 32 -> 0.697
    | 64 -> 0.709
    | m -> 0.7213 /. 1. +. 1.079 /. float m

  let add_item h =
    let idx, w = Misc.split hash_size h p in
    a.(idx) <- max a.(idx) (nlz w - p - 1)

  let count () =
    let e, v =
      let sum, v =
        Array.fold_left (fun (acce, accv) x ->
            acce +. 1. /. (float (1 lsl x)),
            if x = 0 then accv + 1 else accv
          ) (0., 0) a
      in
      alpha_m *. (m * m |> float) /. sum, v
    in
    if e <= 5. *. (float m) /. 2. then
      if v = 0 then e

      (* linear counting*)
      else (float m) *. log ((float m) /. (float v))

    else if e < 1. /. 32. *. (2. ** float hash_size) then
      e
    else ~-. 2.** 32. *. log (1. -. e /. 2. ** 32.)
end
