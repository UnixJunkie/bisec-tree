(* A closed interval *)

type t = { inf: float ;
           sup: float }

let make inf sup =
  assert (inf <= sup);
  { inf; sup }

let is_inside itv x =
  (x >= itv.inf && x <= itv.sup)

let dont_overlap left right =
  let a = left.inf in
  let b = left.sup in
  let c = right.inf in
  let d = right.sup in
  (* [a..b] [c..d] OR [c..d] [a..b] *)
  (b < c) || (d < a)

let overlap left right =
  not (dont_overlap left right)
