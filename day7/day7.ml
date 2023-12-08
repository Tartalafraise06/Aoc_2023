
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



type carte = 
   |Num of int 
   |T
   |J
   |Q
   |K
   |A

type main =
  |High
  |Op
  |Tp
  |Three
  |Full
  |Four
  |Five




let type_main main = 
  let tab = Array.make 15 0 in
  for i = 0 to 4 do
  
  Printf.printf "fini %d \n" main.(i);
    tab.(main.(i)) <- tab.(main.(i)) + 1;
  done;
  Array.sort (Stdlib.compare) tab;
  match tab.(10),tab.(11),tab.(12),tab.(13),tab.(14) with
  |0,0,0,0,5 -> Five
  |0,0,0,1,4 -> Four
  |0,0,0,2,3 -> Full
  |0,0,1,1,3 -> Three
  |0,0,0,2,3 -> Full
  |1,1,1,1,1 -> High
  |0,1,1,1,2 -> Op
  |0,0,1,2,2 -> Tp
  |_ -> failwith "cas de raciste"


let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp
  

let comp_cartes cartes cartes2 =
    let verif = ref true in
    let result = ref false in
    let i = ref 0 in
    while !i < 5 && !verif do
      if cartes.(!i) > cartes2.(!i) then begin verif:= false; result := true end;
      if cartes.(!i) < cartes2.(!i) then verif := false
      else incr i
    done;
    !result


let comp_couple couple couple2 = 
  let main,cartes = (fst couple),(snd couple) in
  let main2,cartes2 = (fst couple2),(snd couple2) in
  if main > main2 then  true 
  else if main < main2 then false
  else comp_cartes cartes cartes2 

  let print_type_main main = 
    match main with
    |High -> print_string "High "
    |Op -> print_string "Op "
    |Tp -> print_string "Tp "
    |Three -> print_string "Three "
    |Full -> print_string "Full "
    |Four -> print_string "Four "
    |Five -> print_string "Five "



let res tab = 
  let c = ref 0 in
  let mains = Array.init (Array.length tab) (fun i -> (0,(High,[||]))) in
  for i = 0 to Array.length tab -1 do 
    let main = List.nth  (String.split_on_char ' ' tab.(i)) 0 in
    let transf_main = Array.init 5 (fun i -> let carte = main.[i] in if is_digit carte then int_of_char carte -48
                                           else match carte with
                                                |'A' -> 14
                                                |'J' -> 11 
                                                |'K' -> 13
                                                |'Q' -> 12  
                                                |'T' -> 10 ) in                         
    let score = List.nth  (String.split_on_char ' ' tab.(i)) 1 in
    mains.(i) <- (int_of_string score,(type_main transf_main,transf_main));
  done;
  for i = 0 to Array.length mains -1 do
    let min_ind = ref i in
    for j = i to Array.length mains -1 do
      if comp_couple (snd mains.(!min_ind)) (snd mains.(j)) then min_ind := j;
    done;
    swap mains !min_ind i;
  done;
    Array.iteri (fun i a -> c:= !c + (fst a)*(i+1)) mains;
  !c

    

let transfo main =
  let max_i = ref 0 in
  let tab = Array.make 15 0 in
  let cop = Array.init 5 (fun i -> main.(i)) in
  for i = 0 to 4 do
    tab.(main.(i)) <- tab.(main.(i)) + 1;
  done;
  for i = 0 to Array.length tab -1 do
    if tab.(i) >= tab.(!max_i) && i<> 11 then max_i := i;
  done;

  for i = 0 to Array.length main -1 do
    if main.(i) = 11 then main.(i) <- 0; 
  done;
 
  if tab.(11) <> 5 then begin  
  for i = 0 to Array.length main -1 do
    if cop.(i) = 11 then cop.(i) <- !max_i;
  done;
 end;
  cop
    


let res2 tab = 
  let c = ref 0 in
  let mains = Array.init (Array.length tab) (fun i -> (0,(High,[||]))) in
  for i = 0 to Array.length tab -1 do 
    let main = List.nth  (String.split_on_char ' ' tab.(i)) 0 in
    let transf_main = Array.init 5 (fun i -> let carte = main.[i] in if is_digit carte then int_of_char carte -48
                                           else match carte with
                                                |'A' -> 14
                                                |'J' -> 11 
                                                |'K' -> 13
                                                |'Q' -> 12  
                                                |'T' -> 10 ) in                         
    let n = transfo transf_main in
    let score = List.nth  (String.split_on_char ' ' tab.(i)) 1 in
    mains.(i) <- (int_of_string score,(type_main n,transf_main));
  done;
  for i = 0 to Array.length mains -1 do
    let min_ind = ref i in
    for j = i to Array.length mains -1 do
      if comp_couple (snd mains.(!min_ind)) (snd mains.(j)) then min_ind := j;
    done;
    swap mains !min_ind i;
  done;
  Array.iteri (fun i a -> c:= !c + (fst a)*(i+1)) mains;
  Array.iteri (fun i a -> print_type_main (fst (snd a)); Printf.printf "rang %d, score %d , \n" i (fst a)) mains;
  !c


let () = 
  let fichier = read_lines (open_in "entree_theo.txt") in
  let tab =  Array.of_list fichier in
(*  let r1 = res tab in *)
  let r2 = res2 tab in 
(* print_int (r1);*)
  print_string "\n \n";
  print_string "le résultat 2 est : ";
  print_int (r2)
