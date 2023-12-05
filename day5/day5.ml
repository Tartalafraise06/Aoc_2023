
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
  | e::l -> Printf.printf "_%s_" e ; print_string "\n" ; print_list l


let seeds tab = 
  let l = List.nth (String.split_on_char ':' tab.(0)) 1 in
  List.filter (fun a -> a<> "") (String.split_on_char ' ' l)

let res tab = 
  let se = ref (List.map (fun a -> int_of_string a) (seeds tab)) in
  let i = ref 1 in 
  while !i < Array.length tab do 
    let suivi = Array.init (List.length (!se)) (fun a -> ref true) in 
    if tab.(!i) <> "" && is_digit tab.(!i).[0] then begin
      while !i < Array.length tab && tab.(!i) <> "" && is_digit tab.(!i).[0] do
        let dest,source,range = Scanf.sscanf tab.(!i) "%d %d %d" (fun a b c -> (a,b,c)) in
        se := List.mapi (fun j a -> if !(suivi.(j)) && a>= source && a < source + range then begin 
                                              suivi.(j):= false;
                                              (a - source + dest) end
        else a ) (!se); 
        incr i;
      done;
    end
    else incr i;
    
  done;
  List.fold_left (fun a b -> Int.min a b) max_int (!se)


let rec transform l =
  match l with
  |[] -> []
  |a::b::q ->  ([|a;b|],true)::(transform q)



let rec print_special l =
  match l with
  |[] -> print_string "\n";
  |a::q -> Printf.printf "(%d,%d)\n " (fst a) (snd a);print_special q



let seeds2 tab = 
  let l = List.nth (String.split_on_char ':' tab.(0)) 1 in
  List.filter (fun a -> a <> "") (String.split_on_char ' ' l)


let cherche_plage couple dest source range = 
  let deb,len = (couple.(0),couple.(1)) in
  Printf.printf "deb %d len %d dest %d source %d range %d \n" deb len dest source range;
  if deb < source && deb + len  >= source && source + range > deb + len  (* --___*) 
   then begin print_string "je rentre dans le 1 !!!! \n "; [([|dest;len + deb - source  |],false);([|deb;source-deb|],true)] end
  else if deb >= source && source + range - 1 < deb+len -1 && source + range -1 > deb (*___---*)
    then begin print_string "je rentre dans le 2 !!!! \n ";[([|deb - source + dest  ;source + range -1 - deb|],false);([|source + range ; deb + len -1 - source - range +1|],true)] end
  else if deb < source && deb + len > source + range(*---____----*)  
    then begin print_string "je rentre dans le 3 !!!! \n "; [([|dest;range|],false);([|deb;deb + len - source + 1|],true);([|source+range;deb+len -source -range +1 |],true)] end
  else if deb >= source && deb + len <= source + range (* ___---____*) 
    then begin   print_string "je rentre dans le 4 !!!! \n "; [([|deb -source + dest  ;len|],false)] end
  else begin  print_string "je rentre dans le 5 !!!! \n ";[([|deb;len|],true)] end

let res2 tab =  
  let se = ref (transform (List.map (fun a -> int_of_string a) (seeds tab))) in
  let i = ref 1 in 
  while !i < Array.length tab do 
    if tab.(!i) <> "" && is_digit tab.(!i).[0] then begin 

      if !i < 15 then Printf.printf "Affichage nouvelle liste : \n";
        List.iter (fun a -> Printf.printf "deb : %d len :%d \n" (fst a).(0) (fst a ).(1)) (!se); 
        Printf.printf "\n \n";
      while !i < Array.length tab && tab.(!i) <> "" && is_digit tab.(!i).[0] do
        let ajout = ref [] in
        let dest,source,range = Scanf.sscanf tab.(!i) "%d %d %d" (fun a b c -> (a,b,c)) in
        se := List.map (fun c -> let plage,verif = c in if verif then begin 
            let l = cherche_plage plage dest source range in

            ajout := (List.tl l) @ (!ajout);
            List.hd l;

                     end
        else (plage,verif) ) (!se); 

        se := !ajout @ (!se);
        incr i;
      done;
      print_string " fin de la ligne \n \n";
      se := List.map (fun a -> let plage,verif = a in (plage,true)) (!se);
    end
    else incr i;
    
  done;
  Printf.printf "taille de la liste : %d " ( List.length (!se));
  (fst (List.fold_left (fun a b -> ([|Int.min (fst a).(0) (fst b).(0)|],false)) ([|max_int;0|],false) (!se))).(0)

 

let () = 
  let fichier = read_lines (open_in "day5.txt") in
  let tab =  Array.of_list fichier in
  let r1 = res tab in 
  print_string "\n \n";
  print_string "le résultat est : ";
  print_int (r1);
  print_string "\n";
  let r2 = res2 tab in 
  print_string "\n \n";
  print_string "le résultat 2 est : \n";
  print_int (r2)
