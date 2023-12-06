
let rec read_lines f =
    try
        let line = input_line f in
        line :: read_lines f
    with End_of_file -> []
 
let is_digit c = 
  (int_of_char c - int_of_char '0') <=9 && (int_of_char c - 48) >= 0



let nombre_of_indice i s = 
  let nb = ref "" in
  if i >= String.length s || i < 0 ||not (is_digit s.[i]) then 
    None
  else begin
    let j = ref 0 in
    while  i+ (!j) < String.length s && is_digit s.[i+ (!j)]  do
      nb := !nb ^ (String.make 1 s.[i+(!j)]);
      incr j; 
   done;
   j := 1;
   while i - (!j) >= 0 && is_digit s.[i-(!j)]  do
      nb := (String.make 1 s.[i-(!j)]) ^ !nb;
      incr j;
    done;
    Some (int_of_string !nb)
  end

let print_int_option = function
  | None -> print_string "None \n"
  | Some i -> Printf.printf "Some %d \n" i
   
let rec print_list = 
  function
    [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l




let res tab = 
  let c = ref 0 in
  let mult = ref 1 in
  let time = [|57;72;69;92|] in
  let dist = [|291;1172;1176;2026|] in
  for i = 0 to Array.length time -1 do 
    for j = 0 to time.(i) do
      let speed = j in
      let left_time = time.(i) - j in
      let travel_dist = left_time * speed in
      if travel_dist > dist.(i) then incr c;
    done;
    mult := !c *(!mult);
    c := 0;
  done;
 !mult 

let res2 tab =  
   let c = ref 0 in
  let mult = ref 1 in
  let time = [|57726992|] in
  let dist = [|291117211762026|] in
  for i = 0 to Array.length time -1 do 
    for j = 0 to time.(i) do
      let speed = j in
      let left_time = time.(i) - j in
      let travel_dist = left_time * speed in
      if travel_dist > dist.(i) then incr c;
    done;
  done;
  !c


let () = 
  let fichier = read_lines (open_in "day6.txt") in
  let tab =  Array.of_list fichier in
  let r1 = res tab in 
  let r2 = res2 tab in 
  print_string "\n \n";
  print_string "le résultat est : ";
  print_int (r1);
  print_string "\n \n";
  print_string "le résultat 2 est : ";
  print_int (r2);
