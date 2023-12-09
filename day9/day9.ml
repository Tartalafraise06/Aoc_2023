
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
  | e::l -> print_int e ; print_string "\n" ; print_list l


let rec verif_nulle l =
  match l with
  |[] -> true
  |t::q -> t=0 && verif_nulle q 

let rec nouvelle_liste l = 
  match l with
  |[] -> [] 
  |a::b::q -> (b-a)::(nouvelle_liste (b::q))
  |t::q -> []


let prediction l =
  let var = ref l in
  let elt = ref [] in
  let compteur = ref 0 in
  while not (verif_nulle (!var)) do
    incr compteur;
    print_list (!var);
    let longueur = List.length (!var) in
    let dernier = List.nth (!var) (longueur -1) in
    elt := dernier::(!elt);
    var := nouvelle_liste (!var);
  done;
  List.fold_left (+) 0 (!elt)
   


 let res tab = 
  let new_tab = Array.map (fun  a -> (List.map (fun b -> int_of_string b) (String.split_on_char ' ' a))) tab in
  Array.fold_left (fun acc elt -> acc + (prediction elt)) (0) new_tab 



let prediction2 l =
  Printf.printf "debut de prediciton :  \n";
  print_list l;
  let var = ref l in
  let elt = ref [] in
  let compteur = ref 0 in
  while not (verif_nulle (!var)) do
    incr compteur;
    print_string "bonjour \n";
    print_list (!var);
    let premier = List.nth (!var) (0) in
    elt := premier::(!elt);
    var := nouvelle_liste (!var);
    print_string "fin du traitement \n";
  done;
  List.fold_left (fun a b -> b - a)  0 (!elt)  
   


let res2 tab = 
  let new_tab = Array.map (fun  a -> (List.map (fun b -> int_of_string b) (String.split_on_char ' ' a))) tab in
  Array.fold_left (fun acc elt -> acc + (prediction2 elt)) (0) new_tab 





let () = 
  print_string "bonjour\n";
  let fichier = read_lines (open_in "day9.txt") in 
  let tab =  Array.of_list fichier in
  print_string "===== START ===== \n";
  let r1 = res tab in 
  let r2 = res2 tab in 
  print_string "\n \n ";
  print_string "le résultat est : ";
  print_int (r1);
  print_string "\n \n";
  print_string "le résultat 2 est : ";
  print_int (r2);
  print_string "\n \n \n  ===== END ===== \n \n \n \n" 
