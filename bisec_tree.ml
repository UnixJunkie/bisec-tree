
module A = MyArray
module L = List

(* Functorial interface *)

module type Point = sig
  type t
  (* dist _must_ be a metric *)
  val dist: t -> t -> float
end

module Make = functor (P: Point) -> struct

  type quality =
    | Very_good (* we use a heuristic to find good vp candidates *)
    | Good (* we use a cheaper heuristic to find good vp candidates *)

  (* The data structure is parametrized by k:
     if there are n <= k points left, we put them
     all in the same bucket. Else, we continue constructing
     the tree recursively.
     This should save storage space and accelerate queries.
     The best value for k is probably dataset and application dependent. *)

  type t = Empty
         | Branch of
             { (* left half-space *)
               l_vp: P.t; (* left vantage point *)
               l_min: float; (* min dist to l_vp in left sub tree *)
               l_max: float; (* max dist to l_vp in left sub tree *)
               l_over: float; (* min dist to l_vp in right sub tree *)
               (* right half-space *)
               r_vp: P.t; (* right vantage point *)
               r_min: float; (* min dist to r_vp in right sub tree *)
               r_max: float; (* max dist to r_vp in right sub tree *)
               r_over: float; (* min dist to r_vp in left sub tree *)
               (* sub-trees *)
               left: t;
               right: t }
         | Bucket of
             { vp: P.t; (* vantage point *)
               dmin: float; (* min dist to vp *)
               dmax: float; (* max dist to vp *)
               points: P.t array } (* remaining points (vp excluded),
                                      ordered by incr. dist. to vp. *)

  let new_bucket vp dmin dmax points =
    Bucket { vp; dmin; dmax; points }

  let new_branch l_vp l_min l_max l_over r_vp r_min r_max r_over left right =
    Branch { l_vp; l_min; l_max; l_over; r_vp; r_min; r_max; r_over; left; right }

  let rng = Random.State.make_self_init ()

  let rand_int n =
    Random.State.int rng n

  (* the very good and good heuristic for choosing a good pair of vp points
     are inspired by section 4.2 'Selecting Split Points' in
     "Near Neighbor Search in Large Metric Spaces", Sergey Brin, VLDB 1995. *)

  let float_compare (x: float) (y: float): int =
    if x < y then -1
    else if x > y then 1
    else 0 (* x = y *)

  (* compare 'p1' and 'p2' by looking at their resp. distance to 'vp' *)
  let order_points vp p1 p2 =
    let d1 = P.dist vp p1 in
    let d2 = P.dist vp p2 in
    float_compare d1 d2

  (* pseudo double normal: we look for a double normal,
     but we don't check we got one *)
  let very_good_vp_pair points =
    let n = Array.length points in
    assert(n >= 2);
    if n = 2 then
      (points.(0), points.(1))
    else
      let i = rand_int n in
      let vp0 = points.(i) in
      Array.sort (order_points vp0) points;
      let vp1 = points.(n - 1) in
      Array.sort (order_points vp1) points;
      let vp2 = points.(n - 1) in
      (vp1, vp2)

  (* choose one vp randomly, the furthest point from it is the other vp *)
  let good_vp_pair points =
    let n = Array.length points in
    assert(n >= 2);
    if n = 2 then
      (points.(0), points.(1))
    else
      let i = rand_int n in
      let vp0 = points.(i) in
      Array.sort (order_points vp0) points;
      let vp1 = points.(n - 1) in
      (vp0, vp1)

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

  let rec to_list t =
    failwith "not implemented yet"

  let neighbors query tol tree =
    failwith "not implemented yet"

  let is_empty = function
    | Empty -> true
    | _ -> false

  let root t =
    failwith "not implemented yet"

  (* test if the tree invariant holds.
     If it doesn't, then we are in trouble... *)
  let rec check t =
    failwith "not implemented yet"

  exception Found of P.t

  let find query tree =
    failwith "not implemented yet"

  let mem query tree =
    failwith "not implemented yet"

  let remove quality query tree =
    failwith "not implemented yet"
end
