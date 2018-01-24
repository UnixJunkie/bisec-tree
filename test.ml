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

(* nearest neighbor brute force search *)
let nearest_brute_force query points =
  let dists = A.map (fun p -> (p, P.dist query p)) points in
  A.sort (fun (p1, d1) (p2, d2) -> compare d1 d2) dists;
  dists.(0)

(* all neighbors within [tol] of [query] using brute force *)
let neighbors_brute_force query tol points =
  let res = ref [] in
  A.iter (fun p -> if P.dist p query <= tol then res := p :: !res) points;
  !res

(* measure time spent in f in seconds *)
let wall_clock_time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  let delta_t = stop -. start in
  (delta_t, res)

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  (* N rand points *)
  let nb_points = 1000 in
  let points = A.init nb_points (fun _ -> P.rand ()) in
  let tree = BST.create points in
  let points' = A.init nb_points (fun _ -> P.rand ()) in
  let tree' = BST.create points' in
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
  );
  (* test neighbors queries *)
  assert(
    A.for_all
      (fun ((x, y) as p) ->
         let tol = Random.State.float rng 1.0 in
         let brute_points = neighbors_brute_force p tol points in
         let smart_points = BST.neighbors p tol tree in
         Log.debug "tol: %f card: %d" tol (L.length smart_points);
         L.sort compare brute_points = L.sort compare smart_points
      ) points
  );
  (* test nearest neighbor queries *)
  assert(
    A.for_all
      (fun p ->
         let brute_points = nearest_brute_force p points' in
         let smart_points = BST.nearest_neighbor p tree' in
         let p', d' = smart_points in
         Log.debug "d': %f" d';
         brute_points = smart_points
      ) points
  );
  (* time construction of tree for many points Vs heuristic *)
  let many_points = A.init 1_000_000 (fun _ -> P.rand ()) in
  let dt1, tree1 = wall_clock_time (fun () -> BST.create many_points) in
  Log.info "dt1: %f" dt1;
  assert(BST.check tree1);
  for i = 1 to 50 do
    let q = P.rand () in
    let dt, (p, d) =
      wall_clock_time (fun () -> nearest_brute_force q many_points) in
    let dt', (p', d') =
      wall_clock_time (fun () -> BST.nearest_neighbor q tree1) in
    assert((p, d) = (p', d'));
    Log.info "dt: %f dt': %f" dt dt'
  done;
  ()

(* FBR: count number of calls to dist using an observed distance *)

(* time queries vs brute force *)

let () = main ()
