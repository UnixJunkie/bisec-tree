
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
  (* The data structure is parametrized by k:
     if there are n <= k points left, we put them
     all in the same bucket. Else, we continue constructing
     the tree recursively.
     This should save storage space and accelerate queries.
     The best value for k is probably dataset and application dependent. *)
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

  type bucket = { vp: P.t; (* vantage point *)
                  (* min and max dist to vp *)
                  bounds: Itv.t;
                  (* remaining points (vp excluded),
                     ordered by incr. dist. to vp. *)
                  points: P.t array }

  type node =
    { (* left half-space *)
      l_vp: P.t; (* left vantage point *)
      l_in: Itv.t; (* dist bounds for points in the same half-space *)
      (* right half-space *)
      r_vp: P.t; (* right vantage point *)
      r_in: Itv.t; (* dist bounds for points in the same half-space *)
      (* sub-trees *)
      left: t;
      right: t }
  and t = Empty
        | Node of node
        | Bucket of bucket

  let new_bucket vp bounds points =
    { vp; bounds; points }

  (* FBR: I will need the itv_intersect code *)

  let new_node l_vp l_in r_vp r_in left right: node =
    { l_vp; l_in; r_vp; r_in; left; right }

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
  (* return dist bounds for vp1 *)
  let min_max1 (points: point2 array): Itv.t =
    let mini = ref points.(0).d1 in
    let maxi = ref points.(0).d1 in
    A.iter (fun x ->
        mini := fmin !mini x.d1;
        maxi := fmax !maxi x.d1
      ) points;
    Itv.make !mini !maxi
  (* return dist bounds for vp2 *)
  let min_max2 (points: point2 array): Itv.t =
    let mini = ref points.(0).d2 in
    let maxi = ref points.(0).d2 in
    A.iter (fun x ->
        mini := fmin !mini x.d2;
        maxi := fmax !maxi x.d2
      ) points;
    Itv.make !mini !maxi

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

  (* sample distances between pair of points in a sample.
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
        let l_in = min_max1 lpoints in
        let r_in = min_max2 rpoints in
        Node (new_node l_vp l_in r_vp r_in
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
      | Node n ->
        (* is l_vp near enough? *)
        let l_d = P.dist query n.l_vp in
        let l_nearby_query = Itv.make (l_d -. tol) (l_d +. tol) in
        let acc' = if l_d <= tol then n.l_vp :: acc else acc in
        (* should we dive left? *)
        let acc'' =
          if Itv.dont_overlap l_nearby_query n.l_in then acc'
          else loop acc' n.left in
        (* is r_vp near enough? *)
        let r_d = P.dist query n.r_vp in
        let r_nearby_query = Itv.make (r_d -. tol) (r_d +. tol) in
        let acc''' = if r_d <= tol then n.r_vp :: acc'' else acc'' in
        (* should we dive right? *)
        if Itv.dont_overlap r_nearby_query n.r_in then acc'''
        else loop acc''' n.right
      | Bucket b ->
        (* is vp near enough? *)
        let d = P.dist b.vp query in
        let acc' =
          if d <= tol then
            b.vp :: acc
          else
            acc in
        (* should we inspect the bucket? *)
        let nearby_query = Itv.make (d -. tol) (d +. tol) in
        if Itv.dont_overlap nearby_query b.bounds then acc'
        else
          A.fold_left (fun accu x ->
              let d' = P.dist query x in
              if d' <= tol then
                x :: accu
              else
                accu
            ) acc' b.points in
    loop [] tree

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

  (* test if the tree invariant holds.
     If it doesn't, we are in trouble... *)
  let rec check = function
    | Empty -> true
    | Bucket b -> (* check bounds *)
      A.for_all (fun x ->
          let d = P.dist b.vp x in
          Itv.is_inside b.bounds d
        ) b.points
    | Node n -> (* check bounds *)
      L.for_all (fun x -> (* lbounds *)
          let d = P.dist n.l_vp x in
          Itv.is_inside n.l_in d
        ) (to_list n.left) &&
      L.for_all (fun x -> (* rbounds *)
          let d = P.dist n.r_vp x in
          Itv.is_inside n.r_in d
        ) (to_list n.right) &&
      (* check left then right *)
      check n.left && check n.right

  exception Found of P.t

  let find query tree =
    let rec loop = function
      | Empty -> ()
      | Bucket b ->
        let d = P.dist b.vp query in
        if d = 0.0 then raise (Found b.vp)
        else if Itv.is_inside b.bounds d then
          (* inspect bucket *)
          A.iter (fun x ->
              if P.dist query x = 0.0 then
                raise (Found x)
            ) b.points
        else () (* no need to check bucket further *)
      | Node n ->
        begin
          let l_d = P.dist n.l_vp query in
          if l_d = 0.0 then raise (Found n.l_vp)
          else if Itv.is_inside n.l_in l_d then
            loop n.left;
          let r_d = P.dist n.r_vp query in
          if r_d = 0.0 then raise (Found n.r_vp)
          else if Itv.is_inside n.r_in r_d then
            loop n.right
        end
    in
    try (loop tree; raise Not_found)
    with Found x -> x

  let mem query tree =
    try let _ = find query tree in true
    with Not_found -> false

end
