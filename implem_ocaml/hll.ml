
module type Params =
sig
  val nlz : int -> int
  val p : int
  val hash_size : int
end

module Make = functor (P: Params) -> struct

  open P

  let m = 1 lsl p
  let mf = float m
  let buckets = Array.make m 0

  let alpha_m = match m with
    | 16 -> 0.673
    | 32 -> 0.697
    | 64 -> 0.709
    | m -> 0.7213 /. 1. +. 1.079 /. float m


  let hashp = hash_size - p

  let reset () = for i = 0 to m - 1 do Array.set buckets i 0 done

  let add_item h =
    let idx = h lsr hashp in
    let w = ((1 lsl (hashp)) - 1) land h in
    buckets.(idx) <- max buckets.(idx) (nlz w - p + 1)
  (* take account of ocaml int size and size of the hash *)


(*
  let count2 () =
  let fd = (open_in "afewresult")
  let rec step () =
    match input_line fd with
    | exception End_of_file -> []
    | l -> let card = read_int in
           ignore input_char;
           let
           :: step ()
  in

  step ()
*)

  let count () =
    (*Array.iter (Format.printf "%d\n") buckets;*)

    let e, v =
      let sum, v =
        Array.fold_left (fun (acce, accv) x ->
            acce +. 1. /. (float @@ 1 lsl x),
            if x = 0 then accv + 1 else accv
          ) (0., 0) buckets
      in
      alpha_m *. mf *. mf /. sum, v
    in
    if e <= 5. *. mf /. 2. then
      if v = 0 then e

      else mf *. log (mf /. (float v)) (* linear counting*)

    else
      let l32 = float @@ 1 lsl 32 in
    if e < 1. /. 32. *. l32 then e
    else ~-. l32 *. log (1. -. e /. l32)
end
