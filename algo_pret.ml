(*djikstra*)
type graph = (int * int) list array

let djikstra (g:graph) (s:int) (t:int) : int =
  let n = Array.length g in
  let dist = Array.make n max_int in
  let visited = Array.make n false in
  let rec loop () =
    let min = ref max_int in
    let min_i = ref (-1) in
    for i = 0 to n-1 do
      if not visited.(i) && dist.(i) < !min then
        (min := dist.(i); min_i := i)
    done;
    if !min_i = -1 then ()
    else
      let u = !min_i in
      visited.(u) <- true;
      List.iter (fun (v,w) ->
        if not visited.(v) && dist.(u) + w < dist.(v) then
          dist.(v) <- dist.(u) + w
      ) g.(u);
      loop ()
  in
  dist.(s) <- 0;
  loop ();
  dist.(t)




