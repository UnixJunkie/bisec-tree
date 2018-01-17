
module A = MyArray
module L = List

(* Functorial interface *)

module type Point = sig
  type t
  (* dist _must_ be a metric *)
  val dist: t -> t -> float
end

module Make = functor (P: Point) -> struct

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
