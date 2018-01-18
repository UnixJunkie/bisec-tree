
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

  (* select one vp randomly, then enrich the points with
     their distance to this vp *)
  let rand_vp points =
    let n = Array.length points in
    assert(n > 0);
    if n = 1 then
      [|(points.(0), 0.0)|]
    else
      let i = rand_int n in
      let vp = points.(i) in
      Array.map (fun p -> (p, P.dist vp p)) points

  (* enrich points with their distance to given vp *)
  let furthest_vp vp enr_points =
    Array.map (fun (p, d0) -> (p, d0, P.dist vp p)) enr_points

  let fst3 (a, _, _) = a
  let snd3 (_, b, _) = b
  let trd3 (_, _, c) = c

  (* pseudo double normal: we look for a double normal,
     but we don't check we actually found one *)
  let very_good_vp_pair points =
    (* FBR: code this one *)
    failwith "not implemented yet"

  (* choose one vp randomly, the furthest point from it is the other vp *)
  let good_vp_pair points =
    let n = Array.length points in
    assert(n >= 2);
    let enr_points = rand_vp points in
    Array.sort (fun x y ->
        float_compare (snd x) (snd y)
      ) enr_points;
    let vp0 = fst enr_points.(n - 1) in
    (* remove selected vp from points array *)
    let enr_rem = A.sub enr_points 0 (n - 1) in
    let enr_points_2 = furthest_vp vp0 enr_rem in
    A.sort (fun x y ->
        float_compare (trd3 x) (trd3 y)
      ) enr_points_2;
    let m = A.length enr_points_2 in
    let vp1 = fst3 enr_points_2.(m - 1) in
    let enr_points_3 = A.sub enr_points_2 0 (m - 1) in
    (vp0, enr_points_3, vp1)

  let bisect (vp0, enr_points, vp1) =
    failwith "not implemented yet"

  let select_vps qual (points: P.t array) =
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
