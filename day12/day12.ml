
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

type couleur = N | B | I  


let est_connu (c:couleur) : bool = match c with
    | I -> false
    | _ -> true


type presolution = couleur array 


let est_complete_lig (p:presolution) : bool =
  let n = Array.length p in 
    let rec aux z = match z with
        | z when z = n -> true
        | z -> (p.(z) <> I) && aux (z+1) in
    aux 0


let trace_lig (p:presolution) : int list = 
    let c = ref 0 in 
    let l = ref [] in
    for z = 0  to Array.length p -1 do
        if p.(z) <> N && !c <> 0 then (l := !l@[!c]; c := 0)
        else if p.(z) = N then incr c
    done;
    if !c <> 0 then l := !l@[!c];
    !l

let est_admissible nombre (p:presolution) : bool = 
    try
        if est_complete_lig p 
        then assert (nombre = trace_lig p);
        true
    with Assert_failure _ -> false


let compter nombre = 
  let compteur = ref  0 in
  List.iter (fun a -> compteur := !compteur +  a ) nombre;
  compteur := !compteur -1;
  !compteur

  let print_couleur a = 
  for i = 0 to Array.length a -1 do
    match a.(i) with
    |I -> print_string "?";
    |B -> print_string ".";
    |N -> print_string "#";
  done


let n_liste l = 
  match l with
  |[] -> [] 
  |1::q -> q 
  |t::q -> (t-1)::q




let calc cases nombre = 
  let long = Array.length cases in
  let total = compter nombre in
    let rec calculer cases nombre n = 
    let longueur = Array.length cases in
    let indice = ref 0 in
    let verif = ref true in
    let new_cases1 = Array.copy cases in
    let new_cases2 = Array.copy cases in
    if est_complete_lig cases  then (   
      if est_admissible nombre cases then 1 
      else 0 )
    else begin
      let resultat = ref 0 in
      while !indice < longueur && !verif do
        if cases.(!indice) = I then begin
          new_cases1.(!indice) <- B ;
          new_cases2.(!indice) <- N ;
          resultat := !resultat + (calculer new_cases1 nombre n) + (calculer new_cases2 nombre (n+1)); 
          verif := false;
          end;
        incr indice;
      done; !resultat
    end
  in
  calculer cases nombre 0 


let calculer2 cases nombre = 
  let long = Array.length cases in
  let dico = Hashtbl.create 500 in
  let rec aux cases nombre i = 
    if i >= long then begin if  (est_admissible nombre cases) then 1 else 0 end
    else ( 
      let avant = Array.sub cases 0 i in
      let apres = Array.sub cases (i+1) (long - i - 1) in
      match cases.(i) with
      |N -> if Hashtbl.mem dico "?" then Hashtbl.find dico "?" else begin
            let resultat = aux cases nombre (i+1) in
            resultat
      end

      |B -> if Hashtbl.mem dico "?" then Hashtbl.find dico "?" else begin 
            let resultat = aux cases nombre (i+1) in 
            resultat
      end

      |I -> let cpN = Array.copy cases in cpB = Array.copy cases in 
            cpN.(i) <- N; 
            cpB.(i) <- B;
            let resultatB = aux cpB nombre (i+1) in
            let resultatN = aux cpN nombre (i+1) in
              resultatN + resultatB
    ) 
  in
  aux cases nombre 0 

let calculer2 les_cases nombre = 
  let memo = Hashtbl.create 42 in
  let nombre_originel = nombre in 
  let long = Array.length les_cases in
  let nombre_a_ajouter = ref 0 in
  List.iter (fun a -> nombre_a_ajouter:= !nombre_a_ajouter + a) nombre;
  (*Le string qu'il reste, le nb de combi, tous les numero qu'il reste à placer et enfin un booléen milieu d'une série de #*)
  let rec aux cases nombre i = 
    Printf.printf "\n"; print_couleur cases; Printf.printf "\n"; 
    let trace_avant = trace_lig (Array.sub cases  0 i ) in

    if i = long then  begin if (est_admissible nombre_originel cases) then 1  else 0 end
    
    else begin 

    let apres = Array.sub cases (i+1) (long - i - 1) in
    let verif = if i > 0 && i < long-1 && cases.(i-1) = N && cases.(i+1) = N  && cases.(i) = N then true else false in

    if Hashtbl.mem memo (i,nombre,trace_avant,apres,verif) then begin  
      Printf.printf "appel dic %d \n" i;
    Hashtbl.find memo (i,nombre,trace_avant,apres,verif) end

    else if nombre = [] then begin if (est_admissible nombre_originel cases) then 1  else 0 end

    else if cases.(i) = N then begin 

      let n = n_liste nombre in
      let resultat = aux cases n (i+1) in
      let n = n_liste nombre in
       Hashtbl.add memo (i+1,n,trace_avant,apres,false) resultat;
     resultat
    end

    else if cases.(i) = B then begin 
    let trace_avant = trace_lig (Array.sub cases  0 i ) in
    let verif = if i > 0 && i < long-1 && cases.(i-1) = N && cases.(i+1) = N then true else false in

    
      let resultat = aux cases nombre (i+1) in Hashtbl.add memo (i+1,nombre,trace_avant,apres,verif) resultat;
    resultat
    end

    else if  cases.(i) = I then begin 
    let verif = if i > 0 && i < long-1 && cases.(i-1) = N && cases.(i+1) = N then true else false in

    let trace_avant = trace_lig (Array.sub cases  0 i ) in

      let case1 = Array.copy cases in
    case1.(i) <- N;

      let n = n_liste nombre in
      let resultat1 = aux case1 (n) (i+1) in
      Hashtbl.add memo (i+1,n,trace_avant,apres,verif) resultat1;
      let cases2 = Array.copy cases in
      cases2.(i) <- B;

      let resultat2 = aux cases2 nombre (i+1) in 
      Hashtbl.add memo (i+1,nombre,trace_avant,apres,false) resultat2;
      resultat1 + resultat2 
    end
    else 0
    end
    

  in 
  aux les_cases nombre 0 



let res tab = 
  let compteur = ref 0 in
  for i = 0 to Array.length tab -1 do 
    let l = String.split_on_char ' ' tab.(i) in
    let c = List.hd l in
    let cases = Array.of_list (List.init (String.length c) (fun i -> 
                                                            match c.[i] with
                                                            | '?' ->  I      
                                                            | '.' -> B
                                                            | '#' -> N
                                                                )) in

    let nombre = List.map (fun a -> int_of_string a) (String.split_on_char ',' (List.nth l 1)) in
    compteur  := !compteur + (calculer2 cases nombre);
    Printf.printf "ligne : %d \n" (i+1) ;
    Printf.printf "\n" ;
    Printf.printf " compteur %d " !compteur;
    Printf.printf "\n";
     
  done;
  !compteur



let res2 tab = 0  (*
  let compteur = ref 0 in
  for i = 0 to Array.length tab -1 do 
    let l = String.split_on_char ' ' tab.(i) in
    let c = List.hd l in
    let str_len = String.length c in
    let cases_l  =  (List.init (str_len ) (fun i ->        match c.[i] with
                                                            | '?' ->  I      
                                                            | '.' -> B
                                                            | '#' -> N
    )) in
    let cases_fin = cases_l @ [I] in
    let cases = Array.of_list (cases_fin @ cases_fin @ cases_fin @ cases_fin @ cases_l) in 
    let nombre_l = List.map (fun a -> int_of_string a) (String.split_on_char ',' (List.nth l 1)) in
    let nombre = nombre_l @ nombre_l @ nombre_l @ nombre_l @ nombre_l in 
    compteur  := !compteur + (calculer2 cases nombre); 
    Printf.printf "ligne : %d \n" (i+1) ;
    Printf.printf "\n" ;
    Printf.printf " compteur %d " !compteur;
    Printf.printf "\n";
  done;
  !compteur
*)


let () = 
  let fichier = read_lines (open_in "tay12.txt") in
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
