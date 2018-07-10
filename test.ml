open Printf

module A = Bst.MyArray
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

module BST = Bst.Bisec_tree.Make (P)

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
  A.fold_left (fun (p, d) p' ->
      let d' = P.dist query p' in
      if d' < d then
        (p', d')
      else
        (p, d)
    ) (query, max_float) points

(* all neighbors within [tol] of [query] using brute force *)
let neighbors_brute_force query tol points =
  A.fold_left (fun acc p ->
      if P.dist query p <= tol then
        p :: acc
      else
        acc
    ) [] points

(* measure time spent in f in seconds *)
let wall_clock_time f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  let delta_t = stop -. start in
  (delta_t, res)

let progress_callback curr total =
  Log.info "done: %.3f" (float curr /. float total)

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  (* N rand points *)
  let nb_points = 1000 in
  let points = A.init nb_points (fun _ -> P.rand ()) in
  let tree_k1 = BST.(create 1 Two_bands) points in
  let tree_k50 = BST.(create 50 Two_bands points) in
  (* check tree lengths *)
  assert(BST.length tree_k1 = nb_points);
  assert(BST.length tree_k50 = nb_points);
  (* check tree invariant *)
  assert(BST.check tree_k1);
  assert(BST.check tree_k50);
  (* addr and add test *)
  let points_1000 = A.init 1000 (fun _ -> P.rand ()) in
  let points_500 = A.sub points_1000 0 500 in
  let tree_500 = BST.(create 1 Two_bands points_500) in
  let tree_1000 = ref tree_500 in
  (* Log.info "tree_500:\n%s" (BST.to_string tree_500); *)
  for i = 500 to 999 do
    let p = points_1000.(i) in
    let addr = BST.get_addr p tree_500 in
    (* Log.info "addr: %s" (Bst.Bisec_tree.string_of_addr addr); *)
    tree_1000 := BST.add p addr !tree_1000;
    assert(BST.check !tree_1000)
  done;
  assert(BST.length tree_500 = 500);
  assert(BST.length !tree_1000 = 1000);
  (* Log.info "tree_1000:\n%s" (BST.to_string !tree_1000); *)
  (* check all points are in the tree *)
  let n = L.length (BST.to_list tree_k1) in
  assert(n = nb_points);
  let m = L.length (BST.to_list tree_k50) in
  assert(n = m);
  Log.info "testing if all points can be found...";
  assert(
    A.for_all
      (fun ((x, y) as p) ->
         let found = BST.mem p tree_k1 in
         if not found then
           Log.error "not found: %f %f" x y;
         found
      ) points
  );
  Log.info "testing neighbor queries...";
  assert(
    A.for_all
      (fun ((x, y) as p) ->
         let tol = Random.State.float rng 1.0 in
         let brute_points = neighbors_brute_force p tol points in
         let smart_points = BST.neighbors p tol tree_k1 in
         Log.debug "tol: %f card: %d" tol (L.length smart_points);
         L.sort compare brute_points = L.sort compare smart_points
      ) points
  );
  Log.info "testing NN queries...";
  assert(
    A.for_all
      (fun p ->
         let brute_points = nearest_brute_force p points in
         let smart_points = BST.nearest_neighbor p tree_k1 in
         brute_points = smart_points
      ) points
  );
  (* time construction of tree for many points Vs heuristic *)
  let many_points = A.init 1_000_000 (fun _ -> P.rand ()) in
  (* inspect distribution of distances *)
  let dists = BST.sample_distances 1000 many_points in
  let dists_fn = "dists_1000.txt" in
  array_to_file dists_fn string_of_float dists;
  let dt1, big_tree =
    wall_clock_time (fun () -> BST.(create 10 Two_bands many_points)) in
  Log.info "dt1: %f" dt1;
  assert(BST.check big_tree);
  Log.info "NN query times";
  for i = 1 to 10 do
    let q = P.rand () in
    let dt, res =
      wall_clock_time (fun () -> nearest_brute_force q many_points) in
    let dt', res' =
      wall_clock_time (fun () -> BST.nearest_neighbor q big_tree) in
    assert(res = res');
    Log.info "dt: %f dt': %f accel: %.1f" dt dt' (dt /. dt')
  done;
  Log.info "neighbors query times";
  for i = 1 to 10 do
    let q = P.rand () in
    let tol = 0.1 in
    let dt, brute_neighbors =
      wall_clock_time (fun () -> neighbors_brute_force q tol many_points) in
    let dt', smart_neighbors =
      wall_clock_time (fun () -> BST.neighbors q tol big_tree) in
    let reference = L.sort compare brute_neighbors in
    assert(reference = L.sort compare smart_neighbors);
    Log.info "dt: %f dt': %f accel: %.1f" dt dt' (dt /. dt');
  done

let () = main ()
