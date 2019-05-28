open Printf

module CLI = Minicli.CLI
module L = BatList

let square x =
  x *. x

module P = struct

  type t = float * float * float

  let dist (x, y, z) (x', y', z') =
    sqrt (square (x -. x') +. square (y -. y') +. square (z -. z'))

  (* avg. list of points to a single point *)
  let average points =
    let n = float (L.length points) in
    let (u, v, w) =
      L.fold_left (fun (x, y, z) (x', y', z') ->
          (x +. x', y +. y', z +. z')
        ) (0., 0., 0.) points in
    (u /. n, v /. n, w /. n)
end

module BST = Bst.Bisec_tree.Make (P)

type filename = string

let with_in_file (fn: filename) (f: in_channel -> 'a): 'a =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let map_on_lines_of_file (fn: filename) (f: string -> 'a): 'a list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> f (input_line input)) in
      if exn = End_of_file then res
      else raise exn
    )

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let _argc, args = CLI.init () in
  let k = CLI.get_int ["-k"] args in
  let points =
    let all_points =
      map_on_lines_of_file "data/bunny.txt" (fun line ->
          Scanf.sscanf line "%f %f %f" (fun x y z -> (x, y, z))
        ) in
    Array.of_list all_points in
  let tree = BST.(create k Two_bands) points in
  let summarized = BST.simplify tree in
  let averaged = L.map P.average summarized in
  L.iter (fun (x, y, z) ->
      printf "%f %f %f\n" x y z
    ) averaged

let () = main ()
