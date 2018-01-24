(* extend stdlib's Array module *)

include Array

(* <=> List.partition *)
let partition p a =
  let n = length a in
  if n = 0 then ([||], [||])
  else
    let ok_count = ref 0 in
    let mask =
      init n (fun i ->
          let pi = p (unsafe_get a i) in
          if pi then incr ok_count;
          pi) in
    let ko_count = n - !ok_count in
    let init = unsafe_get a 0 in
    let ok = make !ok_count init in
    let ko = make ko_count init in
    let j = ref 0 in
    let k = ref 0 in
    for i = 0 to n - 1 do
      let x = unsafe_get a i in
      let px = unsafe_get mask i in
      if px then
        (unsafe_set ok !j x;
         incr j)
      else
        (unsafe_set ko !k x;
         incr k)
    done;
    (ok, ko)
