
open Hll




module P = struct

  let nlz = Misc.nlz
  let p = 6
  let a_m = Misc.a_32

end


module MyHll = Hll.Make (P)



let iter_file s =
  let fd = open_in s in
  let rec step () =
    match input_line fd with
    | exception End_of_file -> MyHll.count ()
    | l -> let h = Hashtbl.hash l in MyHll.add_item h; step ()
  in step ()


let _ =
  Format.printf "result : %f@\n" (iter_file "HungerGames-word.txt")
  (* Format.printf "result : %f@\n" (iter_file "sample.txt") *)
