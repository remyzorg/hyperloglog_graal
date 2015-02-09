



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

let equal_ind3 a b =
  let (a0, a1, a2),(b0, b1, b2) = a, b in
  (b0 = a0 && a1 = b1)

let swap b i1 i2 =
  let open Bytes in
  let t0, t1, t2 = b.[i1], b.[i1 + 1], b.[i1 + 2] in
  set b i1 b.[i2];
  set b (i1 + 1) b.[i2 + 1];
  set b (i1 + 2) b.[i2 + 2];
  set b i2 t0;
  set b (i2 + 1) t1;
  set b (i2 + 2) t2


let filter b =
  let target = Bytes.(create @@ length b) in
  let rec step i j (cind1, cind2, cnlz) =
    if i * 3 = Bytes.length b then j
    else
    let (ind1, ind2, nlz) as bi = get3 b i in
    if ind1 = cind1 && ind2 = cind2 then begin
      let m = (ind1, ind2, max cnlz nlz) in
      set3 target j m;
      step (i + 1) j m
    end else begin
      set3 target (j + 1) bi;
      step (i + 1) (j + 1) bi
    end
  in
  let l = step 1 0 (get3 b 0) in
  let small_buffer = Bytes.create l in
  Bytes.blit target 0 small_buffer 0 l;
  small_buffer



let merge b1 b2 c =
  let b2 = !b2 in
  let b3 = Bytes.(create @@ 3 * (length b1 + length b2)) in
  let rec step i1 i2 i3 v1 v2 =
    let b1_done = i1 > !c in
    let b2_done = i2 >= (Bytes.length b2) in
    if b1_done then
      if b2_done then ()
      else begin
        Bytes.blit b2 (i2 * 3) b3 ((!c + i2) * 3) (Bytes.length b2 - i2)
      end
    else if b2_done then begin
      Bytes.blit b1 (i1 * 3) b3 ((!c + i1) * 3) (Bytes.length b1 - i1)
    end
    else if compare3 v1 v2 then begin
      set3 b3 i3 v1;
      step (i1 + 1) i2 (i3 + 1) (get3 b1 (i1 + 1)) v2
    end else begin
      set3 b3 i3 v2;
      step i1 (i2 + 1) (i3 + 1) v1 (get3 b2 (i2 + 1))
    end
  in step 0 0 0 (get3 b1 0) (get3 b2 0);
  b3


let nlz_string_to_int_array b =
  Array.init ((Bytes.length b) / 3) (fun x ->
      let _,_,nlz = get3 b x in nlz)


let quicksort buff =
  let rec bqs b i j =
    if i = j then ()
    else begin
      let pivot = get3 b (Random.int(j-i) + i) in
      let pos = ref i in
      for k = i to j-1 do
        if compare3 (get3 b k) pivot then begin
          swap b k !pos; pos := !pos + 1
        end
      done;
      bqs b i !pos;
      bqs b !pos j
    end
  in bqs buff 0 (Bytes.length buff/3)

let add p c buffer sorted k v =
  let buffer = !buffer in
  let open Bytes in
  if !c = Bytes.length buffer then begin
    quicksort buffer;
    let merged = merge (filter buffer) sorted c in
    c := 0;
    sorted := merged;
  end;
  let c1, c2 = Misc.split p k 8 in
  set3 buffer !c (c1, c2, v);
  c := !c + 3
