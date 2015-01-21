



module Params = struct
  let nlz = Murmur3.clz64
  let p = 14
  let hash_size = 63 (* 63 Hashtbl.hash doesn't give values over (2^31 - 1) *)
end


module MyHll = Hll.Make (Params)

let input fd =
  let rec step () =
    match input_line fd with
    | exception End_of_file -> MyHll.count ()
    | l -> MyHll.add_item (Murmur3.hash l); step ()
  in step ()


module BenchHll = Hll.Make(Params)

let benchmark_hash ah  =
  let beginning = Unix.gettimeofday () in
  let l = Array.length ah in
  let ext = 1000000000 / l in
  for i = 0 to ext do
    let n = l - 1 in
    for j = 0 to n do
      BenchHll.add_item (ah.(j))
    done;
  done;
  Unix.gettimeofday () -. beginning

let benchmark () =
  Random.init 67;
  let ah = Array.init 10000000 
    (fun n -> Murmur3.hash (string_of_int (Random.int(1000000000)))) in
   (* Array.iter (Format.printf "%d\n") ah; *)
  let v = benchmark_hash ah in
  Format.printf "%f@\n" ( BenchHll.count() );
  v

let benchmark_old () =
  (*37 nanoseconde avec laptop sous batteries*)
  let beginning = Unix.gettimeofday () in
  for i = 0 to 999999999 do
    BenchHll.add_item i
  done;
  Unix.gettimeofday () -. beginning

let () =
     (* Format.printf "%f@\n" (input stdin) ; *)

     Format.printf "%f@\n" (benchmark ())
