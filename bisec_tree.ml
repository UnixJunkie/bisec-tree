
module A = MyArray
module L = List

(* Functorial interface *)

module type Point = sig
  type t
  (* dist _must_ be a metric *)
  val dist: t -> t -> float
end

module Make = functor (P: Point) -> struct

  (* FBR: update *)
  type node = { vp: P.t;
                lb_low: float;
                lb_high: float;
                middle: float;
                rb_low: float;
                rb_high: float;
                left: t;
                right: t }
  and t = Empty
        | Node of node

  let square (x: float): float =
    x *. x

  let float_compare (x: float) (y: float): int =
    if x < y then -1
    else if x > y then 1
    else 0 (* x = y *)

  (* compute distance of point at index 'q_i' to all other points *)
  let distances (q_i: int) (points: P.t array): float array =
    let n = A.length points in
    assert(n > 1);
    let res = A.make (n - 1) 0.0 in
    let j = ref 0 in
    let q = points.(q_i) in
    for i = 0 to n - 1 do
      if i <> q_i then
        (res.(!j) <- P.dist q points.(i);
         incr j)
    done;
    res

  let select_vps (points: P.t array) =
    failwith "not implemented yet"

  let create points =
    failwith "not implemented yet"

  let rec find_nearest acc query tree =
    failwith "not implemented yet"

  let nearest_neighbor query tree =
    match find_nearest None query tree with
    | Some x -> x
    | None -> raise Not_found

  let rec to_list = function
      failwith "not implemented yet"

  let neighbors query tol tree =
    failwith "not implemented yet"

  let is_empty = function
    | Empty -> true
    | Node _ -> false

  let root = function
    | Empty -> raise Not_found
    | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } -> vp

  (* test if the tree invariant holds.
     If it doesn't, then we are in trouble... *)
  let rec check = function
      failwith "not implemented yet"

  exception Found of P.t

  let find query tree =
    failwith "not implemented yet"

  let mem query tree =
    failwith "not implemented yet"

  let remove quality query tree =
    failwith "not implemented yet"
end
