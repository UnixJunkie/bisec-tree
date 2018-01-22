
module A = MyArray
module L = List

(* Functorial interface *)

module type Point = sig
  type t
  (* dist _must_ be a metric *)
  val dist: t -> t -> float
end

(* Apparently, bisector trees where first describbed in
   "A Data Structure and an Algorithm for the Nearest Point Problem";
   Iraj Kalaranti and Gerard McDonald.
   ieeexplore.ieee.org/iel5/32/35936/01703102.pdf *)

module type Config = sig
  val k: int (* bucket size *)
  type quality =
    | Best (* we use brute force to find the diameter of the point set;
              of course, this will not scale in case you have many points *)
    | Good of int (* we use a heuristic to find good vp candidates;
                     Good n => we will try to find a double normal using
                     n optimization steps at most. Optim. stops as soon as a
                     double normal is found. *)
  val q: quality
end

module Make = functor (P: Point) (C: Config) -> struct

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

  let new_bucket vp bounds points =
    { vp; bounds; points }

  (* FBR: I will need the itv_intersect code *)

  let new_node l_vp l_in l_out r_vp r_in r_out left right: node =
    { l_vp; l_in; l_out; r_vp; r_in; r_out; left; right }

  let rng = Random.State.make_self_init ()

  (* n must be > 0 *)
  let rand_int n =
    Random.State.int rng n

  let fcmp (x: float) (y: float): int =
    if x < y then -1
    else if x > y then 1
    else 0 (* x = y *)

  let fmin (x: float) (y: float): float =
    if x < y then x else y

  let fmax (x: float) (y: float): float =
    if x > y then x else y

  (* point indexed by one vantage point *)
  type point1 = { p: P.t;
                  d1: float }
  let point1_cmp (x: point1) (y: point1): int =
    fcmp x.d1 y.d1
  (* enrich p by distance to vp *)
  let enr (vp: P.t) (p: P.t): point1 =
    { p; d1 = P.dist vp p }
  (* point indexed by two vantage points *)
  type point2 = { p: P.t;
                  d1: float;
                  d2: float }
  let point2_cmp1 (x: point2) (y: point2): int =
    fcmp x.d1 y.d1
  let point2_cmp2 (x: point2) (y: point2): int =
    fcmp x.d2 y.d2
  let enr2 (vp: P.t) (p: point1): point2 =
    { p = p.p; d1 = p.d1; d2 = P.dist vp p.p }
  let strip2 (points: point2 array): P.t array =
    A.map (fun x -> x.p) points
  (* return dist bounds for vp1 and vp2 *)
  let min_max12 (points: point2 array): itv * itv =
    let min1 = ref points.(0).d1 in
    let max1 = ref points.(0).d1 in
    let min2 = ref points.(0).d2 in
    let max2 = ref points.(0).d2 in
    A.iter (fun x ->
        min1 := fmin !min1 x.d1;
        max1 := fmax !max1 x.d1;
        min2 := fmin !min2 x.d2;
        max2 := fmax !max2 x.d2
      ) points;
    (new_itv !min1 !max1, new_itv !min2 !max2)
  (* return dist bounds for vp2 *)
  let min_max2 (points: point2 array): itv =
    let mini = ref points.(0).d2 in
    let maxi = ref points.(0).d2 in
    A.iter (fun x ->
        mini := fmin !mini x.d2;
        maxi := fmax !maxi x.d2
      ) points;
    new_itv !mini !maxi

  (* select first vp randomly, then enrich points
     by their distance to it *)
  let rand_vp (points: P.t array): point1 array =
    let n = Array.length points in
    assert(n > 0);
    if n = 1 then
      [|{ p = points.(0); d1 = 0.0 }|]
    else
      let i = rand_int n in
      let vp = points.(i) in
      Array.map (enr vp) points

  (* heuristics for choosing a good pair of vp points
     are inspired by section 4.2 'Selecting Split Points' in
     "Near Neighbor Search in Large Metric Spaces", Sergey Brin, VLDB 1995. *)

  (* choose one vp randomly, the furthest point from it is the other vp;
     output is sorted according to l_vp *)
  let one_band (points: P.t array): P.t * point2 array * P.t =
    let n = Array.length points in
    assert(n >= 2);
    let enr_points = rand_vp points in
    Array.sort point1_cmp enr_points;
    let vp1 = enr_points.(0).p in
    let vp2 = enr_points.(n - 1).p in
    (* remove selected vps from points array
       and enrich points by their dist to vp2 *)
    let enr_rem = A.sub enr_points 1 (n - 2) in
    let rem = Array.map (enr2 vp2) enr_rem in
    (vp1, rem, vp2)

  (* pseudo double normal: we look for a double normal,
     but we don't check if we actually got one;
     output is sorted according to l_vp *)
  let two_bands (points: P.t array): P.t * point2 array * P.t =
    let n = Array.length points in
    assert(n >= 2);
    let enr_points = rand_vp points in
    Array.sort point1_cmp enr_points;
    (* furthest from random vp *)
    let vp = enr_points.(n - 1).p in
    let enr_points1 = Array.map (enr vp) points in
    Array.sort point1_cmp enr_points1;
    (* maybe double normal *)
    let vp1 = enr_points1.(0).p in
    let vp2 = enr_points1.(n - 1).p in
    (* remove selected vps from points array
       and enrich points by their distnce to vp2 *)
    let enr_rem = A.sub enr_points1 1 (n - 2) in
    let rem = Array.map (enr2 vp2) enr_rem in
    (vp1, rem, vp2)

  let heuristic = match C.q with
    | Good 1 -> one_band
    | Good 2 -> two_bands
    | Good _ -> failwith "heuristic: not implemented yet: Good _"
    | Best -> failwith "heuristic: not implemented yet: Best"

  let bucketize (_vp1, enr_points, vp2): bucket =
    (* we use vp2 to index the bucket, because whatever the vp selection
       heuristic, vp2 is supposed to be good while vp1 can be random *)
    let bounds = min_max2 enr_points in
    Array.sort point2_cmp2 enr_points; (* enforce correct sorting of points *)
    let points = strip2 enr_points in
    new_bucket vp2 bounds points

  let rec create (points: P.t array): t =
    let n = Array.length points in
    if n = 0 then Empty
    else
      let enr_points = heuristic points in
      if n <= C.k then Bucket (bucketize enr_points)
      else
        let l_vp, points', r_vp = enr_points in
        (* points to the left are strictly closer to l_vp
           than points to the right *)
        let lpoints, rpoints = A.partition (fun p -> p.d1 < p.d2) points' in
        (* lpoints are sorted by incr. dist. to l_vp,
           but rpoints need to be sorted by incr. dist. to r_vp *)
        Array.sort point2_cmp2 rpoints;
        let l_in, r_out = min_max12 lpoints in
        let l_out, r_in = min_max12 rpoints in
        Node (new_node l_vp l_in l_out r_vp r_in r_out
                (create (strip2 lpoints))
                (create (strip2 rpoints)))

  let rec find_nearest acc query tree =
    failwith "not implemented yet"

  let nearest_neighbor query tree =
    match find_nearest None query tree with
    | Some x -> x
    | None -> raise Not_found

  (* to_list with a nodes acc *)
  let rec to_list_loop acc = function
    | Empty -> acc
    | Node n ->
      let acc' = to_list_loop acc n.right in
      to_list_loop (n.l_vp :: n.r_vp :: acc') n.left
    | Bucket b ->
      A.fold_left (fun acc' x ->
          x :: acc'
        ) (b.vp :: acc) b.points

  let to_list t =
    to_list_loop [] t

  let neighbors query tol tree =
    let rec loop acc = function
      | Empty -> acc
      | Node n -> failwith "not implemented yet"
      | Bucket b -> failwith "not implemented yet"
    in loop [] tree

  let is_empty = function
    | Empty -> true
    | _ -> false

  (* the root is the first point in the vp that we find;
     not sure it is very useful, but at least it allows
     to get one point from the tree if it is not empty *)
  let root = function
    | Empty -> raise Not_found
    | Node n -> n.l_vp
    | Bucket b -> b.vp

  (* test if the tree invariant holds.
     If it doesn't, then we are in trouble... *)
  let rec check t =
    failwith "not implemented yet"

  exception Found of P.t

  let find query tree =
    failwith "not implemented yet"

  let mem query tree =
    failwith "not implemented yet"

end
