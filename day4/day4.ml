
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
    [] -> Printf.printf "fin liste \n" 
  | e::l -> print_string e ; print_string "\n" ; print_list l




let res tab = 
  let c = ref 0 in
  for i = 0 to Array.length tab -1 do 
    let nb_g = ref 0 in 
    let l1 = List.nth (String.split_on_char ':' tab.(i))  1 in
    let l = String.split_on_char '|'l1 in
    let [lEnters;lNum] = List.map (fun a -> String.split_on_char ' ' a) l in
    List.iter (fun a ->  if a = "" then () else if List.mem a lEnters then begin
      if !nb_g = 0 then (nb_g := 1;)
                                                  else nb_g := !nb_g*2 end ) lNum;
    c := !c + (!nb_g);                                
  done;
  !c


let res2 tab = 
  let c = ref (Array.length tab) in
  let scratch = Array.make (Array.length tab) 1 in 

  for i = 0 to Array.length tab -1 do 
      let nb_g = ref 0 in 
      let l1 = List.nth (String.split_on_char ':' tab.(i))  1 in
      let l = String.split_on_char '|'l1 in
      let [lEnters;lNum] = List.map (fun a -> String.split_on_char ' ' a) l in
      List.iter (fun a ->  if a = "" then () 
                    else if List.mem a lEnters then begin
                      incr nb_g;
                                        end ) lNum;
      c := !c + (!nb_g)*scratch.(i);
      for s = 1 to (!nb_g) do
        scratch.(s+ i)<- scratch.(s+i) + scratch.(i);
      done;
  done;
  !c



let () = 
  let fichier = read_lines (open_in "day4.txt") in
  let tab =  Array.of_list fichier in
  let r1 = res tab in 
  let r2 = res2 tab in 
  print_string "\n \n";
  print_string "le résultat est : ";
  print_int (r1);
  print_string "\n \n";
  print_string "le résultat 2 est : ";
  print_int (r2);
