


exception ValueError

open Batteries


let logn s n = Float.(
    (log n) / (log s)
  )

let (<+) = (lsl)
let (+>) = (lsr)

let print_bin v  =
  for i = 31 downto 0 do
    Printf.printf "%d" (v lsr i land 1)
  done;
  print_endline ""

let lsts_n n k = (k <+ n) +> n
let fsts_n n k = (k <+ n) <+ n


let (<+!) x n= x := !x <+ n


let clz x =
  let x = ref x in
  let n = ref 0 in
  if !x <= 0x0000ffff then (n <+! 16; x <+! 16);
  if !x <= 0x00ffffff then (n <+! 8; x <+! 8);
  if !x <= 0x0fffffff then (n <+! 4; x <+! 4);
  if !x <= 0x3fffffff then (n <+! 2; x <+! 2);
  if !x <= 0x7fffffff then incr n;
  !n





module type Data =
  sig
    type t
    val iter : ('a -> unit) -> t -> unit
  end

module Make = functor (D: Data) -> struct

  let aggregation p set hash =
    let htbl = Hashtbl.create 1777 in
    D.iter (fun v ->
        let x = hash v in
        let idx = lsts_n p x in
        let w = fsts_n p x in
        (* let midx = match Hashtbl.find *)
        assert false
    ) set


end
