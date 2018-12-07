
open Printf

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

(* Heuristic to find good vantage points. *)
type vp_heuristic = One_band | Two_bands

type direction = Left | Right

type step = L of float (* dist to l_vp *)
          | R of float (* dist to r_vp *)

let string_of_addr addr =
  let char_of_step = function
    | L _ -> '0'
    | R _ -> '1' in
  let buff = Buffer.create 80 in
  L.iter (fun a ->
      Buffer.add_char buff (char_of_step a)
    ) addr;
  Buffer.contents buff

let string_of_path path =
  let char_of_dir = function
    | Left -> '0'
    | Right -> '1' in
  let buff = Buffer.create 80 in
  L.iter (fun d ->
      Buffer.add_char buff (char_of_dir d)
    ) path;
  Buffer.contents buff

module Make = functor (P: Point) -> struct

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
    let maxi = ref 0.0 in (* a distance is always >= 0.0 *)
    A.iter (fun x ->
        maxi := fmax !maxi x.d1
      ) points;
    !maxi
  (* return max dist to vp2 *)
  let max2 (points: point2 array): float =
    let maxi = ref 0.0 in (* a distance is always >= 0.0 *)
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

  (* counting already indexed points *)
  let pre_bucket_length (b: pre_bucket): int =
    1 + A.length b.points
  let bucket_length (b: bucket): int =
    1 + A.length b.points

  (* select first vp randomly, then enrich points by their distance to it;
     output is ordered by incr. dist. to this rand vp *)
  let rand_vp (points: P.t array): point1 array =
    let n = A.length points in
    assert(n > 0);
    if n = 1 then [|{ p = points.(0); d1 = 0.0 }|]
    else
      let i = rand_int n in
      let vp = points.(i) in
      let enr_points = A.map (enr vp) points in
      A.sort point1_cmp enr_points;
      enr_points

  (* heuristics for choosing a good pair of vp points
     are inspired by section 4.2 'Selecting Split Points' in
     "Near Neighbor Search in Large Metric Spaces", Sergey Brin, VLDB 1995. *)

  (* choose one vp randomly, the furthest point from it is the other vp *)
  let one_band (k: int) (points: P.t array) =
    let n = A.length points in
    if n = 0 then Pre_empty
    else if n = 1 then Pre_bucket { vp = points.(0); points = [||] }
    else (* n >= 2 *)
      let enr_points = rand_vp points in
      let vp1 = enr_points.(0).p in
      let vp2 = enr_points.(n - 1).p in
      let lr_gap = enr_points.(n - 1).d1 in
      (* we bucketize because there are not enough points left, or because
       * it is not possible to bisect space further *)
      if n = 2 || n <= k || lr_gap = 0.0 then
        (* we use vp2 to index the bucket: vp2 is supposed to be good
           while vp1 is random *)
        let enr_rem = A.sub enr_points 0 (n - 1) in
        let rem = A.map (enr2 vp2) enr_rem in
        Pre_bucket { vp = vp2; points = rem }
      else
        (* remove selected vps from point array
           and enrich points by their dist to vp2 *)
        let enr_rem = A.sub enr_points 1 (n - 2) in
        let rem = A.map (enr2 vp2) enr_rem in
        Pre_node { l_vp = vp1; points = rem; r_vp = vp2 }

  let two_bands (k: int) (points: P.t array) =
    let n = A.length points in
    if n = 0 then Pre_empty
    else if n = 1 then Pre_bucket { vp = points.(0); points = [||] }
    else (* n >= 2 *)
      let enr_points = rand_vp points in
      (* furthest from random vp *)
      let vp = enr_points.(n - 1).p in
      let enr_points1 = A.map (enr vp) points in
      A.sort point1_cmp enr_points1;
      let vp1 = enr_points1.(0).p in
      let vp2 = enr_points1.(n - 1).p in
      let lr_gap = enr_points1.(n - 1).d1 in
      (* we bucketize because there are not enough points left, or because
       * it is not possible to bisect space further *)
      if n = 2 || n <= k || lr_gap = 0.0 then
        (* we use vp2 to index the bucket *)
        let enr_rem = A.sub enr_points1 0 (n - 1) in
        let rem = A.map (enr2 vp2) enr_rem in
        Pre_bucket { vp = vp2; points = rem }
      else
        (* remove selected vps from points array
           and enrich points by their distance to vp2 *)
        let enr_rem = A.sub enr_points1 1 (n - 2) in
        let rem = A.map (enr2 vp2) enr_rem in
        Pre_node { l_vp = vp1; points = rem; r_vp = vp2 }

  let sample_distances (sample_size: int) (points: P.t array): float array =
    let n = A.length points in
    assert(n > 0);
    let distances =
      A.init sample_size (fun _ ->
          P.dist points.(rand_int n) points.(rand_int n)
        ) in
    A.sort fcmp distances;
    distances

  let create ?(progress_callback = fun _x _y -> ())
      (k: int) (h: vp_heuristic) (points': P.t array): t =
    let nb_points = A.length points' in
    let indexed = ref 0 in
    let heuristic = match h with
      | One_band -> one_band
      | Two_bands -> two_bands in
    let rec loop points = match heuristic k points with
      | Pre_empty -> Empty
      | Pre_bucket b ->
        begin
          indexed := !indexed + (pre_bucket_length b);
          progress_callback !indexed nb_points;
          Bucket { vp = b.vp; sup = max2 b.points; points = strip2 b.points }
        end
      | Pre_node pn ->
        (* points to the left are strictly closer to l_vp
           than points to the right *)
        let lpoints, rpoints = A.partition (fun p -> p.d1 < p.d2) pn.points in
        indexed := !indexed + 2;
        progress_callback !indexed nb_points;
        Node { l_vp = pn.l_vp;
               l_sup = max1 lpoints;
               r_vp = pn.r_vp;
               r_sup = max2 rpoints;
               left = loop (strip2 lpoints);
               right = loop (strip2 rpoints) } in
    loop points'

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

  let length t =
    let rec loop acc = function
      | Empty -> acc
      | Node n ->
        let acc' = loop acc n.right in
        (* two vantage points --> +2 *)
        loop (acc' + 2) n.left
      | Bucket b ->
        (* one vantage point --> +1 *)
        1 + acc + (A.length b.points) in
    loop 0 t

  (* dive in the tree until [max_depth] is reached
     (or you cannot go further down) then dump all points
     along with the descent path that was followed to reach them *)
  let dump max_depth t =
    let rec loop acc path curr_depth = function
      | Empty -> acc
      | Bucket b ->
        let points = to_list (Bucket b) in
        (L.rev path, points) :: acc
      | Node n ->
        if curr_depth = max_depth then
          let l_points = n.l_vp :: to_list n.left in
          let l_path = Left :: path in
          let r_points = n.r_vp :: to_list n.right in
          let r_path = Right :: path in
          (L.rev l_path, l_points) ::
          (L.rev r_path, r_points) :: acc
        else
          let depth' = curr_depth + 1 in
          let l_path = Left :: path in
          let r_path = Right :: path in
          let acc' = (L.rev l_path, [n.l_vp]) :: acc in
          let acc'' = loop acc' l_path depth' n.left in
          let acc''' = (L.rev r_path, [n.r_vp]) :: acc'' in
          loop acc''' r_path depth' n.right in
    loop [] [] 1 t

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

  (* nearest point to query point *)
  let nearest_neighbor query tree =
    let rec loop ((_x, d) as acc) = function
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

  (* all points [x] such that [P.dist query x <= tol] *)
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
            (* need to inspect further *)
            loop acc' n.left in
        (* should we dive right? *)
        let r_d = P.dist query n.r_vp in
        let acc''' = if r_d <= tol then n.r_vp :: acc'' else acc'' in
        if r_d -. n.r_sup > tol then acc''' (* no *)
        else if r_d +. n.r_sup <= tol then
          (* all remaining points are OK *)
          to_list_loop acc''' n.right
        else
          (* need to inspect further *)
          loop acc''' n.right in
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
    | Node n -> (* check bounds and partitioning rules *)
      L.for_all (fun x -> (* lbounds *)
          let l_d = P.dist n.l_vp x in
          let r_d = P.dist n.r_vp x in
          (l_d <= n.l_sup && l_d < r_d)
        ) (to_list n.left) &&
      L.for_all (fun x -> (* rbounds *)
          let r_d = P.dist n.r_vp x in
          let l_d = P.dist n.l_vp x in
          (r_d <= n.r_sup && r_d <= l_d)
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

  (* find where 'query' would belong in 'tree' *)
  let get_addr query tree =
    let rec loop acc = function
      | Empty | Bucket _ -> L.rev acc
      | Node n ->
        let l_d = P.dist query n.l_vp in
        let r_d = P.dist query n.r_vp in
        if l_d < r_d then
          loop (L l_d :: acc) n.left
        else
          loop (R r_d :: acc) n.right in
    loop [] tree

  (* add 'query' at 'addr' in 'tree' if possible, or crash if not.
     Return a new tree where bounds on the path to the new point
     have been updated. *)
  let add query address tree =
    let rec loop addr = function
      | Empty ->
        begin match addr with
          | [] -> Bucket { vp = query; sup = 0.0; points = [||] }
          | _ -> assert(false) (* cannot go deeper in tree *)
        end
      | Bucket b ->
        begin match addr with
          | [] ->
            let d = P.dist query b.vp in
            let points = A.append b.points [|query|] in
            Bucket { vp = b.vp; sup = max b.sup d; points }
          | _ -> assert(false) (* cannot go deeper in tree *)
        end
      | Node n ->
        begin match addr with
          | [] -> assert(false) (* should go deeper *)
          | L l_d :: rest ->
            Node { l_vp = n.l_vp;
                   l_sup = max n.l_sup l_d;
                   r_vp = n.r_vp;
                   r_sup = n.r_sup;
                   left = loop rest n.left;
                   right = n.right }
          | R r_d :: rest ->
            Node { l_vp = n.l_vp;
                   l_sup = n.l_sup;
                   r_vp = n.r_vp;
                   r_sup = max n.r_sup r_d;
                   left = n.left;
                   right = loop rest n.right }
        end
    in
    loop address tree

  let to_string t =
    let rec loop path acc = function
      | Empty ->
        let str = sprintf "%s 0" (string_of_path (L.rev path)) in
        str :: acc
      | Bucket b ->
        let str =
          sprintf "%s %d"
            (string_of_path (L.rev path))
            (bucket_length b) in
        str :: acc
      | Node n ->
        let acc' = loop (Left :: path) acc n.left in
        loop (Right :: path) acc' n.right
    in
    let unsorted = loop [] [] t in
    let sorted = List.sort compare unsorted in
    String.concat "\n" sorted

  (* let create_sample sample_size nprocs points =
   *   let n = A.length points in
   *   if n <= sample_size then
   *     create 1 Two_bands points
   *   else
   *     begin
   *       (\* randomize points *\)
   *       BatArray.shuffle ~state:rng points;
   *       let to_index = A.sub points 0 sample_size in
   *       let rest_size = n - sample_size in
   *       let rest = A.sub points sample_size rest_size in
   *       (\* create standard bst for the sample *\)
   *       let bst = create 1 Two_bands to_index in
   *       let addr_indexes =
   *         Parmap.array_parmapi ~ncores:nprocs ~chunksize:1
   *           (fun i x -> (i, get_addr x bst)) rest in
   *       (\* add remaining points to previously built bst *\)
   *       A.fold_left (fun acc (i, addr) ->
   *           let point = rest.(i) in
   *           add point addr acc
   *         ) bst addr_indexes
   *     end *)

end
