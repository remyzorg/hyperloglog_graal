


exception ValueError



module type Params =
  sig
    val nlz : int -> int
    val p : int
    val a_m : float
  end

module Make = functor (P: Params) -> struct

  let a = Array.make (1 lsl P.p) 0

  let troll = Array.make 0 false

  let add_item h =

    let idx = Misc.fsts_n P.p h in
    let w = Misc.lsts_n P.p h in
    Format.printf "%d %a\n" idx Misc.print_bin idx;
    (* Format.printf "h: %a\nw  : %a\nl: %a\nidx: %d @\n" Misc.print_bin h Misc.print_bin w Misc.print_bin idx idx; *)
    a.(idx) <- max a.(idx) (P.nlz w)

  let estimate () =

    (* Array.iter (Format.printf "%d\n") a; *)

    let sum = Array.fold_left (
        fun acc x -> 2.0 ** (float_of_int (-x)) +. acc
      ) 0.0 a
    in
    let m_2 = (float_of_int (1 lsl (2 * P.p))) in
    (* Format.printf "%f %f\n" (float_of_int (1 lsl (2 * P.p))) sum; *)
    P.a_m *. m_2 /. sum

  let count () = estimate ()


  let _ =
    Format.printf "size:%d @\n" (Array.length a)



end
