
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

type quality =
  (* | Best (\* we use brute force to find the diameter of the point set;
   *           of course, this will not scale in case you have many points *\) *)
  | Good of int (* we use a heuristic to find good vp candidates;
                   Good n => we will try to find a double normal using
                   n optimization steps at most. Optim. stops as soon as a
                   double normal is found. *)

module type Config = sig
  (* The data structure is parametrized by k:
     if there are n <= k points left, we put them
     all in the same bucket. Else, we continue constructing
     the tree recursively.
     This should save storage space and accelerate queries.
     The best value for k is probably dataset and application dependent.
     k = 0 => the tree is not bucketized *)
  val k: int (* bucket size *)
  val q: quality
end

module Make = functor (P: Point) (C: Config) -> struct

  type bucket = { vp: P.t; (* vantage point *)
                  sup: float; (* max dist to vp among bucket points *)
                  points: P.t array } (* remaining points (vp excluded) *)

  type node =
    { (* left half-space *)
      l_vp: P.t; (* left vantage point *)
      l_sup: float; (* max dist to l_vp among points in same half-space *)
      (* right half-space *)
      r_vp: P.t; (* right vantage point *)
      r_sup: float; (* max dist to r_vp among points in same half-space *)
      (* sub-trees *)
      left: t;
      right: t }
  and t = Empty
        | Node of node
        | Bucket of bucket

  let rng = Random.State.make_self_init ()

  (* n must be > 0 *)
  let rand_int n =
    Random.State.int rng n

  let fcmp (x: float) (y: float): int =
    if x < y then -1
    else if x > y then 1
    else 0 (* x = y *)

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
  let enr2 (vp: P.t) (p: point1): point2 =
    { p = p.p; d1 = p.d1; d2 = P.dist vp p.p }
  let strip2 (points: point2 array): P.t array =
    A.map (fun x -> x.p) points
  (* return max dist to vp1 *)
  let max1 (points: point2 array): float =
    let maxi = ref 0.0 in
    A.iter (fun x ->
        maxi := fmax !maxi x.d1
      ) points;
    !maxi
  (* return max dist to vp2 *)
  let max2 (points: point2 array): float =
    let maxi = ref 0.0 in
    A.iter (fun x ->
        maxi := fmax !maxi x.d2
      ) points;
    !maxi

  (* stuff that will be promoted to bucket *)
  type pre_bucket = { vp: P.t;
                      points: point2 array }
  (* stuff that will be promoted to node *)
  type pre_node = { l_vp: P.t;
                    points: point2 array;
                    r_vp: P.t }
  type pre = Pre_bucket of pre_bucket
           | Pre_node of pre_node
           | Pre_empty

  (* select first vp randomly, then enrich points by their distance to it;
     output is ordered by incr. dist. to this rand vp *)
  let rand_vp (points: P.t array): point1 array =
    let n = Array.length points in
    assert(n > 0);
    if n = 1 then [|{ p = points.(0); d1 = 0.0 }|]
    else
      let i = rand_int n in
      let vp = points.(i) in
      let enr_points = Array.map (enr vp) points in
      Array.sort point1_cmp enr_points;
      enr_points

  (* heuristics for choosing a good pair of vp points
     are inspired by section 4.2 'Selecting Split Points' in
     "Near Neighbor Search in Large Metric Spaces", Sergey Brin, VLDB 1995. *)

  (* choose one vp randomly, the furthest point from it is the other vp *)
  let one_band (points: P.t array) =
    let n = Array.length points in
    if n = 0 then Pre_empty
    else if n = 1 then Pre_bucket { vp = points.(0); points = [||] }
    else if n = 2 then
      Pre_node { l_vp = points.(0); points = [||]; r_vp = points.(1) }
    else (* n > 2 *)
      let enr_points = rand_vp points in
      let vp1 = enr_points.(0).p in
      let vp2 = enr_points.(n - 1).p in
      (* we bucketize because there are not enough points left, or because
       * it is not possible to bisect space further *)
      if n <= C.k || P.dist vp1 vp2 = 0.0 then
        (* we use vp2 to index the bucket: vp2 is supposed to be good
           while vp1 is random *)
        let enr_rem = A.sub enr_points 0 (n - 1) in
        let rem = Array.map (enr2 vp2) enr_rem in
        Pre_bucket { vp = vp2; points = rem }
      else
        (* remove selected vps from points array
           and enrich points by their dist to vp2 *)
        let enr_rem = A.sub enr_points 1 (n - 2) in
        let rem = Array.map (enr2 vp2) enr_rem in
        Pre_node { l_vp = vp1; points = rem; r_vp = vp2 }

  (* pseudo double normal: we look for a double normal,
     but we don't check if we actually got one *)
  let two_bands (points: P.t array) =
    let n = Array.length points in
    if n = 0 then Pre_empty
    else if n = 1 then Pre_bucket { vp = points.(0); points = [||] }
    else if n = 2 then
      Pre_node { l_vp = points.(0); points = [||]; r_vp = points.(1) }
    else (* n > 2 *)
      let enr_points = rand_vp points in
      (* furthest from random vp *)
      let vp = enr_points.(n - 1).p in
      let enr_points1 = Array.map (enr vp) points in
      Array.sort point1_cmp enr_points1;
      (* maybe double normal *)
      let vp1 = enr_points1.(0).p in
      let vp2 = enr_points1.(n - 1).p in
      (* we bucketize because there are not enough points left, or because
       * it is not possible to bisect space further *)
      if n <= C.k || P.dist vp1 vp2 = 0.0 then
        (* we use vp2 to index the bucket *)
        let enr_rem = A.sub enr_points1 0 (n - 1) in
        let rem = Array.map (enr2 vp2) enr_rem in
        Pre_bucket { vp = vp2; points = rem }
      else
        (* remove selected vps from points array
           and enrich points by their distance to vp2 *)
        let enr_rem = A.sub enr_points1 1 (n - 2) in
        let rem = Array.map (enr2 vp2) enr_rem in
        Pre_node { l_vp = vp1; points = rem; r_vp = vp2 }

  let heuristic = match C.q with
    | Good 1 -> one_band
    | Good 2 -> two_bands
    | Good _ -> failwith "heuristic: not implemented yet: Good _"
    (* | Best -> failwith "heuristic: not implemented yet: Best" *)

  (* sample distances between all distinct points in a sample.
     The result is sorted. *)
  let sample_distances (sample_size: int) (points: P.t array): float array =
    let n = A.length points in
    (* draw with replacement *)
    let sample = A.init sample_size (fun _ -> points.(Random.int n)) in
    let distances = A.make (sample_size * (sample_size - 1) / 2) 0.0 in
    let k = ref 0 in
    for i = 0 to sample_size - 2 do
      for j = i + 1 to sample_size - 1 do
        distances.(!k) <- P.dist sample.(i) sample.(j);
        incr k
      done;
    done;
    A.sort fcmp distances;
    distances

  let rec create (points: P.t array): t =
    match heuristic points with
    | Pre_empty -> Empty
    | Pre_bucket b ->
      Bucket { vp = b.vp; sup = max2 b.points; points = strip2 b.points }
    | Pre_node pn ->
      (* points to the left are strictly closer to l_vp
         than points to the right *)
      let lpoints, rpoints = A.partition (fun p -> p.d1 < p.d2) pn.points in
      Node { l_vp = pn.l_vp;
             l_sup = max1 lpoints;
             r_vp = pn.r_vp;
             r_sup = max2 rpoints;
             left = create (strip2 lpoints);
             right = create (strip2 rpoints) }

  (* to_list with an acc *)
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

  let is_empty = function
    | Empty -> true
    | _ -> false

  (* the root is the first point in the vp that we find
     (either a bucket's vp or a node's left vp);
     not sure it is very useful, but at least it allows
     to get one point from the tree if it is not empty *)
  let root = function
    | Empty -> raise Not_found
    | Node n -> n.l_vp
    | Bucket b -> b.vp

  let nearest_neighbor query tree =
    let rec loop ((x, d) as acc) = function
      | Empty -> acc
      | Bucket b ->
        let b_d = P.dist query b.vp in
        let x', d' = if b_d < d then (b.vp, b_d) else acc in
        (* should we inspect bucket points? *)
        if b_d -. b.sup >= d' then (x', d') (* no *)
        else (* yes *)
          A.fold_left (fun (nearest_p, nearest_d) y ->
              let y_d = P.dist query y in
              if y_d < nearest_d then (y, y_d) else (nearest_p, nearest_d)
            ) (x', d') b.points
      | Node n ->
        let l_d = P.dist query n.l_vp in
        let x', d' = if l_d < d then (n.l_vp, l_d) else acc in
        (* should we dive left? *)
        let x'', d'' =
          if l_d -. n.l_sup >= d' then (x', d') (* no *)
          else loop (x', d') n.left (* yes *) in
        (* should we dive right? *)
        let r_d = P.dist query n.r_vp in
        let x''', d''' = if r_d < d'' then (n.r_vp, r_d) else (x'', d'') in
        if r_d -. n.r_sup >= d''' then (x''', d''') (* no *)
        else loop (x''', d''') n.right (* yes *) in
    match tree with
    | Empty -> raise Not_found
    | not_empty ->
      let x = root not_empty in
      loop (x, P.dist query x) not_empty

  (* return all points [x] such that [P.dist query x <= tol] *)
  let neighbors query tol tree =
    let rec loop acc = function
      | Empty -> acc
      | Bucket b ->
        let b_d = P.dist query b.vp in
        let acc' = if b_d <= tol then b.vp :: acc else acc in
        (* should we inspect bucket points? *)
        if b_d -. b.sup > tol then acc' (* no *)
        else if b_d +. b.sup <= tol then
          (* all remaining points are OK *)
          A.fold_left (fun accu x ->
              x :: accu
            ) acc' b.points
        else (* we need to inspect the bucket *)
          A.fold_left (fun acc'' y ->
              let y_d = P.dist query y in
              if y_d <= tol then y :: acc'' else acc''
            ) acc' b.points
      | Node n ->
        let l_d = P.dist query n.l_vp in
        let acc' = if l_d <= tol then n.l_vp :: acc else acc in
        (* should we dive left? *)
        let acc'' =
          if l_d -. n.l_sup > tol then acc' (* no *)
          else if l_d +. n.l_sup <= tol then
            (* all remaining points are OK *)
            to_list_loop acc' n.left
          else
            loop acc' n.left in
        (* should we dive right? *)
        let r_d = P.dist query n.r_vp in
        let acc''' = if r_d <= tol then n.r_vp :: acc'' else acc'' in
        if r_d -. n.r_sup > tol then acc''' (* no *)
        else if r_d +. n.r_sup <= tol then
          to_list_loop acc''' n.right
        else loop acc''' n.right in
    loop [] tree

  (* test if the tree invariant holds.
     If it doesn't, we are in trouble... *)
  let rec check = function
    | Empty -> true
    | Bucket b -> (* check bounds *)
      A.for_all (fun x ->
          let d = P.dist b.vp x in
          d <= b.sup
        ) b.points
    | Node n -> (* check bounds *)
      L.for_all (fun x -> (* lbounds *)
          let d = P.dist n.l_vp x in
          d <= n.l_sup
        ) (to_list n.left) &&
      L.for_all (fun x -> (* rbounds *)
          let d = P.dist n.r_vp x in
          d <= n.r_sup
        ) (to_list n.right) &&
      (* check left then right *)
      check n.left && check n.right

  (* extract vp points from the tree *)
  let inspect tree =
    let rec loop acc = function
      | Empty -> acc
      | Bucket b -> b.vp :: acc
      | Node n ->
        let acc' = n.l_vp :: n.r_vp :: acc in
        let acc'' = loop acc' n.left in
        loop acc'' n.right in
    loop [] tree

  let find query tree =
    let nearest_p, nearest_d = nearest_neighbor query tree in
    (* Log.warn "nearest_d: %f" nearest_d; *)
    if nearest_d = 0.0 then nearest_p else raise Not_found

  let mem query tree =
    try let _ = find query tree in true
    with Not_found -> false

end
