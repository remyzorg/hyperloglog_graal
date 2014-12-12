


exception ValueError

open Batteries


let logn s n = Float.(
    (log n) / (log s)
  )


let print_bin v  =
  for i = 31 downto 0 do
    Printf.printf "%d" (v lsr i land 1)
  done

let (<+) = (lsl)
let (+>) = (lsr)

module type Data =
  sig
    type t
    val iter : ('a -> unit) -> t -> unit
  end

module Make = functor (D: Data) -> struct

  let aggregation p set hash =
    D.iter (fun v ->
        let x = hash v in
        let idx = (x +> p) <+ p in
        let w = (x <+ p) +> p in
        let midx = max
    ) set


end
