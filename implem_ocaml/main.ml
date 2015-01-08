
open Hll




module P = struct

  let nlz = Misc.nlz
  let p = 16
  let alpha_m = Misc.a_32
  let hash_size = 30

end


module MyHll = Hll.Make (P)



let iter_file s =
  let fd = open_in s in
  let rec step () =
    match input_line fd with
    | exception End_of_file -> MyHll.count ()
    | l -> MyHll.add_item (Hashtbl.hash l); step ()
  in step ()


let _ =
  Format.printf "result : %f@\n" (iter_file "HungerGames-word.txt")
  (* Format.printf "result : %f@\n" (iter_file "sample.txt") *)
