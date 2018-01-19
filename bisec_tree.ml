
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

  (* closed interval *)
  type itv = { inf: float ;
               sup: float }

  let new_itv inf sup =
    assert (inf <= sup);
    { inf; sup }

  (* The data structure is parametrized by k:
     if there are n <= k points left, we put them
     all in the same bucket. Else, we continue constructing
     the tree recursively.
     This should save storage space and accelerate queries.
     The best value for k is probably dataset and application dependent. *)

  type bucket = { vp: P.t; (* vantage point *)
                  (* min and max dist to vp *)
                  bounds: itv;
                  (* remaining points (vp excluded),
                     ordered by incr. dist. to vp. *)
                  points: P.t array }

  let new_bucket vp bounds points =
    { vp; bounds; points }

  type node =
    { (* left half-space *)
      l_vp: P.t; (* left vantage point *)
      l_in: itv; (* dist bounds for points in the same half-space *)
      l_out: itv; (* dist bounds for points in the other half-space *)
      (* right half-space *)
      r_vp: P.t; (* right vantage point *)
      r_in: itv; (* dist bounds for points in the same half-space *)
      r_out: itv; (* dist bounds for points in the other half-space *)
      (* sub-trees *)
      left: t;
      right: t }
  and t = Empty
        | Node of node
        | Bucket of bucket

  let new_node l_vp l_in l_out r_vp r_in r_out left right =
    { l_vp; l_in; l_out; r_vp; r_in; r_out; left; right }

  let rng = Random.State.make_self_init ()

  (* n must be > 0 *)
  let rand_int n =
    Random.State.int rng n

  (* the very good and good heuristic for choosing a good pair of vp points
     are inspired by section 4.2 'Selecting Split Points' in
     "Near Neighbor Search in Large Metric Spaces", Sergey Brin, VLDB 1995. *)

  let float_compare (x: float) (y: float): int =
    if x < y then -1
    else if x > y then 1
    else 0 (* x = y *)

  (* point known by one vantage point *)
  type point1 = { p: P.t;
                  d1: float }
  let point1_cmp x y =
    float_compare x.d1 y.d1
  (* enrich p by distance to vp *)
  let enr (vp: P.t) (p: P.t): point1 =
    { p; d1 = P.dist vp p }
  (* point known by two vantage points *)
  type point2 = { p: P.t;
                  d1: float;
                  d2: float }
  let point2_cmp1 x y =
    float_compare x.d1 y.d1
  let point2_cmp2 x y =
    float_compare x.d2 y.d2
  let enr2 (vp: P.t) (p: point1): point2 =
    { p = p.p; d1 = p.d1; d2 = P.dist vp p.p }

  (* select first vp randomly, then enrich points
     by their distance to it *)
  let rand_vp points =
    let n = Array.length points in
    assert(n > 0);
    if n = 1 then
      [|{ p = points.(0); d1 = 0.0 }|]
    else
      let i = rand_int n in
      let vp = points.(i) in
      Array.map (enr vp) points

  (* choose one vp randomly, the furthest point from it is the other vp *)
  let good_vp_pair points =
    let n = Array.length points in
    assert(n >= 2);
    let enr_points = rand_vp points in
    Array.sort point1_cmp enr_points;
    let vp1 = enr_points.(0).p in
    let vp2 = enr_points.(n - 1).p in
    (* remove selected vps from points array
       and enrich points by their distnce to vp2 *)
    let enr_rem = A.sub enr_points 1 (n - 2) in
    let rem = Array.map (enr2 vp2) enr_rem in
    (vp1, rem, vp2)

  (* pseudo double normal: we look for a double normal,
     but we don't check we actually found one *)
  let very_good_vp_pair points =
    (* FBR: code this one *)
    failwith "not implemented yet"

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
