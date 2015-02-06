



let p = 14

let sorted = Bytes.create (1 lsl p * 3)

let buffer = Bytes.create (1 lsl p * 3)

let cpt = ref 0

let set3 b i (c1, c2, c3) =
  let open Bytes in
  Char.chr c1 |> set b (i * 3);
  set b (i * 3 + 1) @@ Char.chr c2 ;
  set b (i * 3 + 2) @@ Char.chr c3

let get3 b i =
  let open Bytes in
  let i = i * 3 in
  (int_of_char b.[i], int_of_char b.[i + 1], int_of_char b.[i + 2])

let compare3 a b =
  let (a0, a1, a2),(b0, b1, b2) = a, b in
  a0 < b0 || (b0 = a0 && a1 <= b1)


let add k v =
  let c = !cpt in
  let open Bytes in
  if c = Bytes.length buffer then assert false
  else
    let c1, c2 = Misc.split p k 8 in
    set3 buffer c (c1, c2, v);
    cpt := !cpt + 3


let swap b i1 i2 =
  let open Bytes in
  let t0, t1, t2 = b.[i1], b.[i1 + 1], b.[i1 + 2] in
  set b i1 b.[i2];
  set b (i1 + 1) b.[i2 + 1];
  set b (i1 + 2) b.[i2 + 2];
  set b i2 t0;
  set b (i2 + 1) t1;
  set b (i2 + 2) t2



let quicksort buff =
let rec bqs b i j =
  let pivot = get3 b (Random.int(j-i) + i) in
  let pos = ref i in
  for k = i to j-1 do
    if compare3 (get3 b k) pivot then begin
      swap b k !pos; pos := !pos + 1
    end
  done;
  bqs b i !pos;
  bqs b !pos j;
  in bqs buff 0 (Bytes.length buff/3)
