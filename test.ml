open Printf

module A = Array
module L = List

let square x =
  x *. x

let rng = Random.State.make_self_init ()

module P = struct
  type t = float * float
  let dist (x, y) (x', y') =
    sqrt (square (x -. x') +. square (y -. y'))
  let rand () =
    (Random.State.float rng 1.0, Random.State.float rng 1.0)
end

module C = struct
  let k = 50
  let q = Bisec_tree.Good 2
end

module BST = Bisec_tree.Make (P) (C)

let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let list_to_file fn to_string l =
  with_out_file fn (fun out ->
      L.iter (fun x -> fprintf out "%s\n" (to_string x)) l
    )

let array_to_file fn to_string a =
  with_out_file fn (fun out ->
      A.iter (fun x -> fprintf out "%s\n" (to_string x)) a
    )

let main () =
  Log.color_on ();
  Log.set_log_level Log.DEBUG;
  (* N rand points *)
  let nb_points = 1000 in
  let points = A.init nb_points (fun _ -> P.rand ()) in
  let tree = BST.create points in
  (* check tree invariant *)
  assert(BST.check tree);
  let dists = BST.sample_distances (nb_points / 10) points in
  let dists_fn = "test.dists" in
  array_to_file dists_fn string_of_float dists;
  (* check all points are in the tree *)
  let n = L.length (BST.to_list tree) in
  assert(n = nb_points);
  (* test all points can be found back *)
  assert(
    A.for_all
      (fun ((x, y) as p) ->
         let found = BST.mem p tree in
         if not found then
           Log.error "not found: %f %f" x y;
         found
      ) points
  )
  (* neighbors *)
  (* nearest_neighbor *)

let () = main ()
