
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

let abs a b = 
  if a - b >= 0 then a - b 
  else b - a


let distances couple1 couple2 colonne ligne = 
  let compter = ref 0 in
  let i1,j1 = couple1 in
  let i2,j2 = couple2 in
  List.iter (fun a -> if (i1 < a  && a < i2) || (i2 < a && a < i1) then incr compter) ligne;
  List.iter (fun a -> if (j1 < a  && a < j2) || (j2 < a && a < j1) then incr compter) colonne;
  (abs (i1) (i2) + abs (j1) (j2) + !compter) 

  
  



let res tab = 
  let mat = Array.make_matrix (Array.length tab) (String.length tab.(0)) '.' in
  let c = ref 0 in
  let l = String.length tab.(0) in
  let colonne = ref [] in
  let ligne = ref [] in
  let galaxies = ref [] in
  for i = 0 to Array.length tab -1 do 
    let verif = ref true in
    for j = 0 to l -1 do
      let elt = tab.(i).[j] in
      if elt = '#' then begin verif := false; galaxies := (i,j)::(!galaxies); end;
      mat.(i).(j) <- elt;
    done;
    if !verif then ligne := i::(!ligne)
    done;
  for i = 0 to Array.length mat.(0) -1 do
    let verif = ref true in
    for j = 0 to Array.length mat -1 do
      let elt = mat.(j).(i) in
      if elt = '#' then verif := false;
    done;
    if !verif then colonne := i::(!colonne)
  done;
  print_list (!colonne);
  Printf.printf "\n";
  print_list (!ligne);
  let d = distances (0,3) (1,9) !colonne !ligne in
  Printf.printf "la distance est : %d \n " d;
  let total = ref 0 in
  List.iter (fun couple1 -> 
    List.iter (fun couple2 -> if couple1 <> couple2 then 
    let d = distances couple1 couple2 !colonne !ligne  in total := d + (!total); ) (!galaxies); 
  Printf.printf "nouveau_total : %d \n " (!total);) (!galaxies);
  !total/2 



let distances couple1 couple2 colonne ligne = 
  let compter = ref 0 in
  let i1,j1 = couple1 in
  let i2,j2 = couple2 in
  List.iter (fun a -> if (i1 < a  && a < i2) || (i2 < a && a < i1) then incr compter) ligne;
  List.iter (fun a -> if (j1 < a  && a < j2) || (j2 < a && a < j1) then incr compter) colonne;
  (abs (i1) (i2) + abs (j1) (j2) + (999999*(!compter)))



let res2 tab =  
  let mat = Array.make_matrix (Array.length tab) (String.length tab.(0)) '.' in
  let c = ref 0 in
  let l = String.length tab.(0) in
  let colonne = ref [] in
  let ligne = ref [] in
  let galaxies = ref [] in
  for i = 0 to Array.length tab -1 do 
    let verif = ref true in
    for j = 0 to l -1 do
      let elt = tab.(i).[j] in
      if elt = '#' then begin verif := false; galaxies := (i,j)::(!galaxies); end;
      mat.(i).(j) <- elt;
    done;
    if !verif then ligne := i::(!ligne)
    done;
  for i = 0 to Array.length mat.(0) -1 do
    let verif = ref true in
    for j = 0 to Array.length mat -1 do
      let elt = mat.(j).(i) in
      if elt = '#' then verif := false;
    done;
    if !verif then colonne := i::(!colonne)
  done;
  print_list (!colonne);
  Printf.printf "\n";
  print_list (!ligne);
  let d = distances (0,3) (1,9) !colonne !ligne in
  let total = ref 0 in
  List.iter (fun couple1 -> 
    List.iter (fun couple2 -> if couple1 <> couple2 then 
    let d = distances couple1 couple2 !colonne !ligne  in total := d + (!total); ) (!galaxies); 
  Printf.printf "nouveau_total : %d \n " (!total);) (!galaxies);
  !total/2 
  

let () = 
  let fichier = read_lines (open_in "day11.txt") in
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
