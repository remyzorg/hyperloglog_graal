


exception ValueError



module type Params =
  sig
    val nlz : int -> int
    val p : int
    val alpha_m : float
    val hash_size : int
  end

module Make = functor (P: Params) -> struct

  let word_size = Sys.word_size

  let a = Array.make (1 lsl P.p) 0

  let troll = Array.make 0 false

  let first_n = Misc.first_n P.hash_size
  let last_n = Misc.last_n word_size P.hash_size

  let nlz x = P.nlz x - (P.hash_size - P.p) - (32 - P.hash_size)

  let add_item h =
    let idx = first_n P.p h in
    let w = last_n P.p h in
    let qw = nlz w in
    (* Format.printf "%a %d\n%a %d\n\n" Misc.print_bin h h Misc.print_bin w w; *)
    (* if idx = 0 then *)
    (* Format.printf "h: %a\nf  : %a\nl: %a\nidx: %d\nqw: %d \n@\n" Misc.print_bin h Misc.print_bin w Misc.print_bin idx idx qw; *)
    a.(idx) <- max a.(idx) qw

  let estimate () =

    (* Array.iter (Format.printf "%d\n") a; *)

    let sum =
      Array.fold_left (fun acc x ->
        2.0 ** (float_of_int ~-x) +. acc
      ) 0.0 a
    in

    let m_2 = float_of_int @@ 1 lsl (2 * P.p) in
    (* Format.printf "%f %f\n" (float_of_int (1 lsl (2 * P.p))) sum; *)

    P.alpha_m *. m_2 /. sum

  let count () = estimate ()


  let _ =
    Format.printf "size:%d @\n" (Array.length a)

end
