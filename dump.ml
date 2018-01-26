open Printf

module A = Array
module L = BatList

let square x =
  x *. x

let rng = Random.State.make_self_init ()

module P3D = struct
  type t = float * float * float
  let dist (x, y, z) (x', y', z') =
    sqrt (square (x -. x') +. square (y -. y') +. square (z -. z'))
  let rand () =
    (Random.State.float rng 1.0, Random.State.float rng 1.0, Random.State.float rng 1.0)
  let of_string s =
    Scanf.sscanf s "%f %f %f" (fun x y z -> x, y, z)
  let to_string (x, y, z) =
    sprintf "%f %f %f" x y z
end

let qual2 = Bisec_tree.Good 2

module C = struct
  let k = 10
  let q = qual2
end
module BST = Bisec_tree.Make (P3D) (C)

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let lines_of_file fn =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      if exn <> End_of_file then
        raise exn
      else res
    )

let char_of_dir = function
  | Bisec_tree.Left -> '0'
  | Bisec_tree.Right -> '1'

let string_of_path p =
  let buff = Buffer.create 80 in
  L.iter (fun dir ->
      Buffer.add_char buff (char_of_dir dir)
    ) p;
  Buffer.contents buff

let main () =
  let argc, args = CLI.init () in
  let input_fn = CLI.get_string ["-i"] args in
  let depth = CLI.get_int ["-d"] args in
  let point_lines = lines_of_file input_fn in
  let points = L.map P3D.of_string point_lines in
  let tree = BST.create (A.of_list points) in
  let dump = BST.dump depth tree in
  L.iter (fun (path, points) ->
      let p_str = string_of_path path in
      L.iter (fun p ->
          printf "%s %s\n" p_str (P3D.to_string p)
        ) points
    ) dump

let () = main ()
