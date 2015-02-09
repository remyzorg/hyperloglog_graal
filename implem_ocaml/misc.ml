

(* useless but fancy operator *)
let (<+) = (lsl)
let (+>) = (lsr)
let (<+!) x n= x := !x <+ n
let (+!) x n = x := !x + n


(* print int in bit format *)
let print_bin fmt v  =
  for i = 62 downto 0 do
    Format.fprintf fmt "%d" (v lsr i land 1);
    if i = 30 then Format.fprintf fmt " ";
    if i = 15 then Format.fprintf fmt " ";
  done


(* split an int in two parts : first n bits (shifted to right), lasts size - n bits *)
let split size i n = i +> (size - n), ((1 <+ (size - n)) - 1) land i

let firsts size i n = i +> (size - n)
let lasts size i n = ((1 <+ (size - n)) - 1) land i



(* Number of leading zeros from Hacker's Delight book page
   http://www.hackersdelight.org/hdcodetxt/nlz.c.txt *)
let nlz x =
  if x = 0 then 32 else
    let x = ref x in
    let n = ref 0 in
    if !x <= 0x0000ffff then (n +! 16; x <+! 16);
    if !x <= 0x00ffffff then (n +! 8; x <+! 8);
    if !x <= 0x0fffffff then (n +! 4; x <+! 4);
    if !x <= 0x3fffffff then (n +! 2; x <+! 2);
    if !x <= 0x7fffffff then incr n;
    !n


let dicho_search t f =
  let rec step i j =
    if i = j + 1 || i = j then i, j
    else
      let mid = (i + j / 2) in
      if f >= snd t.(mid) then
        step mid j
      else step i mid
  in step 0 (Array.length t / 2)

let linear_interpolation t (i, j) f =
  let inti, fli = t.(i) in
  let intj, flj = t.(i) in
  float inti +. (float @@ intj - inti) *. ((f -. fli) /. (flj -. fli))
