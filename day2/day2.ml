
let rec read_lines f =
    try
        let line = input_line f in
        line :: read_lines f
    with End_of_file -> []


let rec print_list = 
  function
    [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l

let is_digit c = 
  (int_of_char c - int_of_char '0') <=9 && (int_of_char c - 48) >= 0

let recup s num = 
  (*let taille =( if num <9 then 1 else if num <= 98 then 2 else 3) in
  let id = String.init taille  (fun i -> s.[i+5]) in*)
  let r_max = ref 0 in
  let g_max = ref 0 in
  let b_max = ref 0 in
  let l = List.nth (String.split_on_char ':' s) 1  in
  let tirage = String.split_on_char ';' l in 
  List.iter (fun a -> print_string a; print_string ";;;;" ) tirage;
  let tirage = List.map (fun a -> String.split_on_char ',' a) tirage in
  List.iter (fun a -> List.iter (fun x -> Scanf.sscanf x " %d %s " (fun i s -> Printf.printf "je lis ,%i,%s,\n " i s;  
                                  match s with 
                                  |"red" ->(Printf.printf "ancien rmax : %d \n" !r_max;if i > !r_max then r_max := i;Printf.printf "nouveau rmax : %d \n" !r_max;)
                                  |"green" -> if i > !g_max then g_max := i;
                                  |"blue" -> if i > !b_max then b_max := i;
                                  |_ -> print_string "mauvaise lecture \n")) a ) tirage;
  [|!r_max;!g_max;!b_max|]



let res tab = 
  let c = ref 0 in
  for i = 0 to Array.length tab -1 do 
    let a = recup tab.(i) i in
    if a.(0) <= 12 && a.(1) <= 13 && a.(2) <= 14 then c := !c + i + 1;
    
  done;
  !c 

let recup2 s num = 
  (*let taille =( if num <9 then 1 else if num <= 98 then 2 else 3) in
  let id = String.init taille  (fun i -> s.[i+5]) in*)
  let r_max = ref 0 in
  let g_max = ref 0 in
  let b_max = ref 0 in
  let l = List.nth (String.split_on_char ':' s) 1  in
  let tirage = String.split_on_char ';' l in 
  List.iter (fun a -> print_string a; print_string ";;;;" ) tirage;
  let tirage = List.map (fun a -> String.split_on_char ',' a) tirage in
  List.iter (fun a -> List.iter (fun x -> Scanf.sscanf x " %d %s " (fun i s -> Printf.printf "je lis ,%i,%s,\n " i s;  
                                  match s with 
                                  |"red" ->(Printf.printf "ancien rmax : %d \n" !r_max;if  !r_max = 0 || i < !r_max then r_max := i;Printf.printf "nouveau rmax : %d \n" !r_max;)
                                  |"green" -> if !g_max = 0 || i < !g_max then g_max := i;
                                  |"blue" -> if !b_max = 0 || i < !b_max then b_max := i;
                                  |_ -> print_string "mauvaise lecture \n")) a ) tirage;

  [|!r_max;!g_max;!b_max|]




let res2 tab =  
  let c = ref 0 in
  for i = 0 to Array.length tab -1 do 
    let a = recup tab.(i) i in
    c:= !c + a.(0)*a.(1)*a.(2);
    
  done;
  !c 


let () = 
  let fichier = read_lines (open_in "day2.txt") in
  let tab =  Array.of_list fichier in
  let r1 = res tab in 
  let r2 = res2 tab in 
  print_string "\n \n";
  print_string "le résultat est : ";
  print_int (r1);
  print_string "\n \n";
  print_string "le résultat 2 est : ";
  print_int (r2);
