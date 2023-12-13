
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
    [] ->print_string "vide \n"; 
  | e::l -> print_int e ; print_string "," ; print_list l


let check_sym_ligne mat li = 
  let long = Array.length mat in
  let i = ref 0 in
  let verif = ref true in
  while li -(!i) >= 0 && li + (!i) + 1 < long do
    if mat.(li - (!i) ) <> mat.(li + (!i) + 1) then verif:= false; 
    incr i;
  done;
  !verif
    

let check_sym_colonne mat ci = 
  let long = String.length mat.(0) in
  let a = Array.length mat in
  let i = ref 0 in
  let verif = ref true in
  while ci -(!i)  >= 0 && ci + (!i) + 1 < long do
    let colonne_g = String.init a (fun j -> mat.(j).[ci - (!i)]) in
    let colonne_d = String.init a (fun j -> mat.(j).[ci + (!i) + 1]) in
    if colonne_g <> colonne_d then  verif := false;
    incr i;
  done;
  !verif
    

let cherche mat = 
  let ty = Array.length mat in
  let tx = String.length mat.(0) in
  let c = ref 0 in
  for i = 0 to Array.length mat -2 do 
    if check_sym_ligne mat i then c := !c + 100*(i + 1);
  done;
  for j = 0 to String.length mat.(0) -2 do
    if check_sym_colonne mat j then c := !c + (j+1);
  done;
  !c


let res tab = 
  let c = ref 0 in
  let i= ref 0 in
  let l_tab = Array.length tab in
  while !i < l_tab do
    if tab.(!i) = "" then incr i
    else begin
      let l = ref [] in
      while !i < l_tab && tab.(!i) <> "" do
        l:= tab.(!i) :: (!l);
        incr i;
      done;
      c  := !c + cherche (Array.of_list (List.rev (!l)));  
    end;
  done;
  !c

let cherche2 mat = 
  let ty = Array.length mat in
  let tx = String.length mat.(0) in
  let c = ref 0 in
  let ligne = ref [] in
  let colonne = ref [] in
  for i = 0 to Array.length mat -2 do 
    if check_sym_ligne mat i then begin
      c := !c + 100*(i + 1);
      ligne := (i+1) :: (!ligne);
    end
  done;
  for j = 0 to String.length mat.(0) -2 do
    if check_sym_colonne mat j then begin
      c := !c + (j+1);
    colonne := (j+1) :: (!colonne);
    end;
  done;
  (!c, !ligne, !colonne)





let check_smudge mat = 
  let ancien_score,a_ligne,a_colonne = cherche2 mat in
  let taille_s = String.length mat.(0) in
  let verif = ref true in
  let new_res = ref ancien_score in
  for i = 0 to Array.length mat -1 do
    for j = 0 to String.length mat.(0) -1 do
      if !verif then begin
        let cp = Array.copy mat in
        cp.(i) <- String.init taille_s (fun a -> if a <> j then mat.(i).[a] else match mat.(i).[a] with
                                                                                 |'.' -> '#'
                                                                                 |'#' -> '.');
(*      Array.iter (fun a -> print_string a;print_string "\n") cp;*)
      let score, ligne, colonne = cherche2 cp in 
(*      Printf.printf "%d %d \n" i j;
      print_list ligne;
      print_list colonne; *)
      let colonne_m = ref colonne in
      let ligne_m = ref ligne in 
      if a_ligne <> [] then 
        ligne_m := List.filter (fun a -> a <> List.hd a_ligne) ligne;
      if a_colonne <> [] then 
        colonne_m := List.filter (fun a -> a <> List.hd a_colonne) colonne; 
      if (ligne <> [] && ligne <> a_ligne ) || (colonne <> [] && colonne <> a_colonne) then begin
        verif := false;
        new_res := 0;
        List.iter (fun a -> new_res := !new_res + 100*a) !ligne_m;
        List.iter (fun a -> new_res := !new_res + a) !colonne_m;

      end
    end;
    done
  done;
  !new_res


let res2 tab =  
  let c = ref 0 in
  let i= ref 0 in
  let l_tab = Array.length tab in
  while !i < l_tab do
    if tab.(!i) = "" then incr i
    else begin
      let l = ref [] in
      while !i < l_tab && tab.(!i) <> "" do
        l:= tab.(!i) :: (!l);
        incr i;
      done;
      c  := !c + (check_smudge (Array.of_list (List.rev (!l))));  
    end;
  done;
  !c


let () = 
  let fichier = read_lines (open_in "day13.txt") in
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
