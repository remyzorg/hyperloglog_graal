



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

let gen_data card = 
  Array.init card (fun n -> Murmur3.hash (string_of_int (Random.int(1000000000))))





let benchmark_old () =
  (*37 nanoseconde avec laptop sous batteries*)
  let beginning = Unix.gettimeofday () in
  for i = 0 to 999999999 do
    BenchHll.add_item i
  done;
  Unix.gettimeofday () -. beginning

let experiments pallier maxcard = 
  let n = maxcard/pallier in
  (*pour toutes les cardinalité de pallier en pallier*)
  for i = 1 to n do
    let card = (i*pallier) in
    let res = Array.init 1000 
      (fun _ ->
        (* génere #card données, et estime leur cardinalité *)
        let ah = gen_data card in
        Array.iter BenchHll.add_item ah;
        BenchHll.count()
      )
     in
     (*cardinalité  moyenne_estim  mediane_estim  pct01_estim  pct99_estim*)
     Array.sort compare res;
     let moy_est = (Array.fold_left (+.) 0.0 res) /. float_of_int (Array.length res) in
     let med_est = res.(499) in
     let pct01_est = res.(9) in
     let pct99_est = res.(989) in
     Format.printf "%d\t%f\t%f\t%f\t%f\n" card moy_est med_est pct01_est pct99_est
  done
  
let () =
     experiments 500 100000;
     (* Format.printf "%f@\n" (input stdin) ; *)

     (*Format.printf "%f@\n" (benchmark ()) *)
