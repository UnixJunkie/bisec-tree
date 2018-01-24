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

let qual1 = Bisec_tree.Good 1
let qual2 = Bisec_tree.Good 2

module C = struct
  let k = 50
  let q = qual2
end
module BST = Bisec_tree.Make (P) (C)

module C0 = struct
  let k = 0
  let q = qual2
end
module BST0 = Bisec_tree.Make (P) (C0)

module C1 = struct
  let k = 1
  let q = qual2
end
module BST1 = Bisec_tree.Make (P) (C1)

module C2 = struct
  let k = 2
  let q = qual2
end
module BST2 = Bisec_tree.Make (P) (C2)

module C5 = struct
  let k = 5
  let q = qual2
end
module BST5 = Bisec_tree.Make (P) (C5)

module C10 = struct
  let k = 10
  let q = qual2
end
module BST10 = Bisec_tree.Make (P) (C10)

module C20 = struct
  let k = 20
  let q = qual2
end
module BST20 = Bisec_tree.Make (P) (C20)

module C50 = struct
  let k = 50
  let q = qual2
end
module BST50 = Bisec_tree.Make (P) (C50)

module C100 = struct
  let k = 100
  let q = qual2
end
module BST100 = Bisec_tree.Make (P) (C100)

module C200 = struct
  let k = 200
  let q = qual2
end
module BST200 = Bisec_tree.Make (P) (C200)

module C500 = struct
  let k = 500
  let q = qual2
end
module BST500 = Bisec_tree.Make (P) (C500)

module C1000 = struct
  let k = 1000
  let q = qual2
end
module BST1000 = Bisec_tree.Make (P) (C1000)

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
  let dt1, big_tree = wall_clock_time (fun () -> BST.create many_points) in
  Log.info "dt1: %f" dt1;
  assert(BST.check big_tree);

  let tree0 = BST0.create many_points in
  assert(BST0.check tree0);
  let tree1 = BST1.create many_points in
  assert(BST1.check tree1);
  let tree2 = BST2.create many_points in
  assert(BST2.check tree2);
  let tree5 = BST5.create many_points in
  assert(BST5.check tree5);
  let tree10 = BST10.create many_points in
  assert(BST10.check tree10);
  let tree20 = BST20.create many_points in
  assert(BST20.check tree20);
  let tree50 = BST50.create many_points in
  assert(BST50.check tree50);
  let tree100 = BST100.create many_points in
  assert(BST100.check tree100);
  let tree200 = BST200.create many_points in
  assert(BST200.check tree200);
  let tree500 = BST500.create many_points in
  assert(BST500.check tree500);
  let tree1000 = BST1000.create many_points in
  assert(BST1000.check tree1000);

  Log.info "NN query times";
  for i = 1 to 10 do
    let q = P.rand () in
    let dt, res =
      wall_clock_time (fun () -> nearest_brute_force q many_points) in
    let dt', res' =
      wall_clock_time (fun () -> BST.nearest_neighbor q big_tree) in
    assert(res = res');
    Log.info "dt: %f dt': %f" dt dt'
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
    Log.info "dt: %f dt': %f" dt dt';
    (* try other BST configs *)
    let dt0, sn0 = wall_clock_time (fun () -> BST0.neighbors q tol tree0) in
    assert(reference = L.sort compare sn0);
    Log.info "dt0: %f" dt0;
    let dt1, sn1 = wall_clock_time (fun () -> BST1.neighbors q tol tree1) in
    assert(reference = L.sort compare sn1);
    Log.info "dt1: %f" dt1;
    let dt2, sn2 = wall_clock_time (fun () -> BST2.neighbors q tol tree2) in
    assert(reference = L.sort compare sn2);
    Log.info "dt2: %f" dt2;
    let dt5, sn5 = wall_clock_time (fun () -> BST5.neighbors q tol tree5) in
    assert(reference = L.sort compare sn5);
    Log.info "dt5: %f" dt5;
    let dt10, sn10 = wall_clock_time (fun () -> BST10.neighbors q tol tree10) in
    assert(reference = L.sort compare sn10);
    Log.info "dt10: %f" dt10;
    let dt20, sn20 = wall_clock_time (fun () -> BST20.neighbors q tol tree20) in
    assert(reference = L.sort compare sn20);
    Log.info "dt20: %f" dt20;
    let dt50, sn50 = wall_clock_time (fun () -> BST50.neighbors q tol tree50) in
    assert(reference = L.sort compare sn50);
    Log.info "dt50: %f" dt50;
    let dt100, sn100 = wall_clock_time (fun () -> BST100.neighbors q tol tree100)
    in
    assert(reference = L.sort compare sn100);
    Log.info "dt100: %f" dt100;
    let dt200, sn200 = wall_clock_time (fun () -> BST200.neighbors q tol tree200)
    in
    assert(reference = L.sort compare sn200);
    Log.info "dt200: %f" dt200;
    let dt500, sn500 = wall_clock_time (fun () -> BST500.neighbors q tol tree500)
    in
    assert(reference = L.sort compare sn500);
    Log.info "dt500: %f" dt500;
    let dt1000, sn1000 =
      wall_clock_time (fun () -> BST1000.neighbors q tol tree1000) in
    assert(reference = L.sort compare sn1000);
    Log.info "dt1000: %f" dt1000
  done;
  ()

(* FBR: count number of calls to dist using an observed distance *)

let () = main ()
