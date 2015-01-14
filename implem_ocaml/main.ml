


module MyHll = Hll.Make (
struct
  let nlz = Misc.nlz
  let p = 14
  let hash_size = 30 (* Hashtbl.hash doesn't give values over (2^31 - 1) *)
end)

let input fd =
  let rec step () =
    match input_line fd with
    | exception End_of_file -> MyHll.count ()
    | l -> MyHll.add_item (Hashtbl.hash l); step ()
  in step ()

let () =
  Format.printf "%f@\n" (input stdin)
