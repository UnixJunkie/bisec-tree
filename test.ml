
module A = Array

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

let main () =
  let nb_points = 1000 in
  let points = A.init nb_points (fun _ -> P.rand ()) in
  let tree = BST.create points in
  assert(BST.check tree)

let () = main ()
