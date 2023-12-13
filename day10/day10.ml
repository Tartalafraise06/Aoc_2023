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



let search mat i j = 
  if i < Array.length mat && j < Array.length mat.(0) && j >= 0 && i >= 0 then
    mat.(i).(j)
  else 
    (".",true)

let change mat i j s = 
  if i < Array.length mat && j < Array.length mat.(0) && j >= 0 && i >= 0 then
    mat.(i).(j) <- s



let print_queue q = 
  (*print une file sans la modifier*)
  let q2 = Queue.copy q in  
  while not (Queue.is_empty q2) do
    let i,j,_ = Queue.take q2 in
    Printf.printf "(%d,%d) " i j;
  done;
  print_string "\n"

let chercher i_start j_start mat = 
  let file = Queue.create () in
  let max = ref 0 in
  let rec aux () = 
    if Queue.is_empty file then ()
    else begin 
    let i,j,compteur = Queue.take file in
    let pipe,_ = search mat i j in
    if compteur > !max then max := compteur;
    match pipe with
    |"|" -> begin change mat i j ("|",true); let _,verif1 = search mat (i-1) j in if not verif1 then  
                                            Queue.add ((i-1),j,(compteur+1)) file; 
                                            let _,verif2 = search mat (i+1) j in if not verif2 then
                                            Queue.add ((i+1),j,(compteur+1)) file; 
                                            aux () end

    |"-" -> begin change mat i j ("-",true); let _,verif1 = search mat (i) (j-1) in if not verif1 then  
                                            Queue.add ((i),(j-1),(compteur+1)) file; 
                                            let _,verif2 = search mat (i) (j+1) in if not verif2 then begin
                                            Queue.add ((i),(j+1),(compteur+1)) file; end;
                                            aux () end

    |"L" -> begin change mat i j ("L",true); let _,verif1 = search mat (i-1) (j) in if not verif1 then  
                                            Queue.add ((i-1),(j),(compteur+1)) file; 
                                            let _,verif2 = search mat (i) (j+1) in if not verif2 then
                                            Queue.add ((i),(j+1),(compteur+1)) file; 
                                            aux () end


    |"J" -> begin change mat i j ("J",true); let _,verif1 = search mat (i-1) (j) in if not verif1 then  
                                            Queue.add ((i-1),(j),(compteur+1)) file; 
                                            let _,verif2 = search mat (i) (j-1) in if not verif2 then
                                            Queue.add ((i),(j-1),(compteur+1)) file;
                                            aux () end

    |"7" -> begin change mat i j ("7",true); let _,verif1 = search mat (i+1) (j) in if not verif1 then  
                                            Queue.add ((i+1),(j),(compteur+1)) file; 
                                            let _,verif2 = search mat (i) (j-1) in if not verif2 then
                                            Queue.add ((i),(j-1),(compteur+1)) file; 
                                            aux () end

    |"F" -> begin change mat i j ("F",true); let _,verif1 = search mat (i+1) (j) in if not verif1 then  
                                            Queue.add ((i+1),(j),(compteur+1)) file; 
                                            let _,verif2 = search mat (i) (j+1) in if not verif2 then
                                            Queue.add ((i),(j+1),(compteur+1)) file; 
                                            aux () end

    |"." -> begin change mat i j (".",true); aux () end 
    |"S" -> begin change mat i j ("S",true); let _,verif1 = search mat (i+1) (j) in if not verif1 then  
                                            Queue.add ((i+1),(j),(compteur+1)) file; 
                                            let _,verif2 = search mat (i) (j-1) in if not verif2 then
                                            Queue.add ((i),(j-1),(compteur+1)) file; 
                                            aux () end
    |cara  -> failwith "caractère inconnu " 
    end;
    in
    Queue.add (i_start,j_start,0) file;
    aux (); 
    !max 

let res tab = 
  let mat = Array.make_matrix (Array.length tab) (String.length tab.(0)) ("",false) in
  let long = String.length tab.(0) in 
  let i_start = ref 0 in
  let j_start = ref 0 in
  for i = 0 to Array.length tab -1 do 
    for j = 0 to long -1 do
      if tab.(i).[j] = 'S' then begin i_start:= i; j_start := j end
    done;
    mat.(i) <- Array.of_list (List.init (long) (fun j ->  ( (String.make 1 tab.(i).[j]),false)   ));
  done;
  Printf.printf "depart : \n";
  chercher (!i_start) (!j_start) mat

let res2 tab =  
  let mat = Array.make_matrix (Array.length tab) (String.length tab.(0)) ("",false) in
  let long = String.length tab.(0) in 
  let i_start = ref 0 in
  let j_start = ref 0 in
  for i = 0 to Array.length tab -1 do 
    for j = 0 to long -1 do
      if tab.(i).[j] = 'S' then begin i_start:= i; j_start := j end
    done;
    mat.(i) <- Array.of_list (List.init (long) (fun j ->  ( (String.make 1 tab.(i).[j]),false)   ));
  done;
  Printf.printf "depart : \n";
  let res1 = chercher (!i_start) (!j_start) mat in
  let tot = ref 0 in

  for i = 0 to Array.length mat -1 do
    for j = 0 to Array.length mat.(0) - 1 do
      let indice = ref j in
      let nombre_mur = ref 0 in
      let dernier = "" in
      if not (snd mat.(i).(j)) then begin 
        Printf.printf "je rentre \n";
        while !indice >= 0 do
          let pipe, is_in_loop = mat.(i).(!indice) in
          if is_in_loop then begin
            if List.mem pipe ["|";"L";"7";"J";"F";"S"] then
              if dernier 
              incr nombre_mur; 
          end;
        indice := !indice -1; 
        done;
      Printf.printf "%d \n" !nombre_mur;
      end;
    if !nombre_mur mod 2  = 1 then incr tot;
      done;
  done; 
  !tot 


let () = 
  Printf.printf "bonjour ";
  let fichier = read_lines (open_in "tay10.txt") in
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
