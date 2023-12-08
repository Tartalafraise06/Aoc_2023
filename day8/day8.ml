
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
  let instr = tab.(0) in
  let c = ref 0 in
  let map = Hashtbl.create (Array.length tab) in
  for i = 1 to Array.length tab -1 do 
    if tab.(i) <> "" then begin
      let l = String.split_on_char '=' tab.(i) in
      let deb = String.trim (List.nth l 0) in
      let fin_part = String.trim (List.nth l 1) in
      let a = String.sub fin_part 1 3 in
      let b = String.sub fin_part 6 3 in
      Hashtbl.add map (deb ^ "L")  a; 
      Hashtbl.add map (deb ^ "R")  b
    end;

    () 
  done;
  let current = ref "AAA" in
  let verif = ref true in
  while !verif do
    for i = 0 to String.length instr -1 do
      current := Hashtbl.find map ((!current) ^ (String.make 1 (instr.[i])));
    incr c;
    done;
    if !current = "ZZZ" then verif := false;
  done;
  !c

let rec pgcd n m =
  if n > m then pgcd m n
  else if n = 0 then m
       else let r = m mod n in
            pgcd r n

let ppcm n m =
  (n*m)/(pgcd n m)



let compter deb map instr =
  let c = ref 0 in
  let current = ref deb in
  let verif = ref true in
  while !verif do
    for i = 0 to String.length instr -1 do
      current := Hashtbl.find map ((!current) ^ (String.make 1 (instr.[i])));
    incr c;
    done;
    if (!current).[2] = 'Z' then verif := false;
  done;
  !c



let res2 tab =
  let instr = tab.(0) in
  let c = ref 1 in
  let map = Hashtbl.create (Array.length tab) in
  for i = 1 to Array.length tab -1 do 
    if tab.(i) <> "" then begin
      let l = String.split_on_char '=' tab.(i) in
      let deb = String.trim (List.nth l 0) in
      let fin_part = String.trim (List.nth l 1) in
      let a = String.sub fin_part 1 3 in
      let b = String.sub fin_part 6 3 in
      Hashtbl.add map (deb ^ "L")  a; 
      Hashtbl.add map (deb ^ "R")  b
    end;

    () 
  done;
  for i = 2 to Array.length tab - 1 do
    let l = String.sub tab.(i) 0 3 in
    if l.[2] = 'A' then begin
      let num = compter l map instr in
      c:= ppcm (!c) num;
    end;
  done;
  !c

let () = 
  let fichier = read_lines (open_in "day8.txt") in
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
