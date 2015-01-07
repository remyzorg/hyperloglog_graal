


open Batteries


let (<+) = (lsl)
let (+>) = (lsr)

let print_bin fmt v  =
  for i = 63 downto 0 do
    Format.fprintf fmt "%d" (v lsr i land 1);
    if i = 32 then Format.fprintf fmt "."
  done;
  print_endline ""

let lsts_n n k = (k <+ (63 - n)) +> 31
let fsts_n n k = k +> (32 - n)


let (<+!) x n= x := !x <+ n
let (+!) x n = x := !x + n

let a_32 = 0.709

let nlz x =
  let x = ref x in
  let n = ref 0 in
  if !x <= 0x0000ffff then (n +! 16; x <+! 16);
  if !x <= 0x00ffffff then (n +! 8; x <+! 8);
  if !x <= 0x0fffffff then (n +! 4; x <+! 4);
  if !x <= 0x3fffffff then (n +! 2; x <+! 2);
  if !x <= 0x7fffffff then incr n;
  !n
