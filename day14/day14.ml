
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


let swap arr i1 j1 i2 j2 = 
  let temp = arr.(i1).(j1) in
  arr.(i1).(j1) <- arr.(i2).(j2);
  arr.(i2).(j2) <- temp



let res tab = 
  let c = ref 0 in
  let lx = Array.length tab in
  let ly = String.length tab.(0) in
  let a = Array.make_matrix lx ly "" in 

  for i = 0 to lx - 1 do
    for j = 0 to ly -1 do

      a.(i).(j) <- String.make 1 tab.(i).[j]
    done
  done;

  for i = 0 to lx - 1 do
    for j = 0 to ly -1 do
      let indice = ref i in 
        while !indice > 0 && a.(!indice-1).(j) = "." do
          indice := !indice -1
        done;
      if a.(i).(j) = "O" then 
        swap a (!indice) j i j
    done;
  done;
  for i = 0 to lx-1 do
    for j = 0 to ly-1 do 
      if a.(i).(j) = "O" then
        c := !c + lx - i;
    done;
  done;
  !c

let a_print a = 
  let lx = Array.length a in
  let ly = Array.length a.(0) in
 
   for i = 0 to lx-1 do
    for j = 0 to ly-1 do 
    print_string a.(i).(j);
    done;
    print_string "\n"
  done
 

let cycle a = 
(* north *)
  let lx = Array.length a in
  let ly = Array.length a.(0) in
  for i = 0 to lx - 1 do
    for j = 0 to ly -1 do
      let indice = ref i in 
        while !indice > 0 && a.(!indice-1).(j) = "." do
          indice := !indice -1
        done;
      if a.(i).(j) = "O" then 
        swap a (!indice) j i j
    done;
  done;

  (* west *)
  for j = 0 to ly-1 do
    for i = 0 to lx - 1 do
      let indice = ref j in 
        while !indice > 0  && a.(i).(!indice -1 ) = "." do
          indice := !indice -1
        done;
      if a.(i).(j) = "O" then 
        swap a i (!indice)  i j
    done;
  done;

(* south *)
  for i = lx - 1 downto 0 do
    for j = 0 to ly -1 do
      let indice = ref i in 
        while !indice < lx -1  && a.(!indice+1).(j) = "." do
          indice := !indice +1
        done;
      if a.(i).(j) = "O" then 
        swap a (!indice) j i j
    done;
  done;
(* east *)

  for j = ly -1 downto 0 do
    for i = 0 to lx - 1 do
      let indice = ref j in 
        while !indice < ly -1  && a.(i).(!indice +1 ) = "." do
          indice := !indice +1
        done;
      if a.(i).(j) = "O" then 
        swap a  i (!indice)  i j
    done;
  done

let res2 tab = 
  let c = ref 0 in
  let lx = Array.length tab in
  let ly = String.length tab.(0) in
  let a = Array.make_matrix lx ly "" in 

  for i = 0 to lx - 1 do
    for j = 0 to ly -1 do
      a.(i).(j) <- String.make 1 tab.(i).[j]
    done
  done;

  let compteur = ref 0 in
  let dico  = Hashtbl.create 5000 in
  let debut = Array.init lx (fun i -> Array.copy a.(i)) in

  a_print a;
  print_string "\n \n";
  while not (Hashtbl.mem dico a) do
    let kle = Array.init lx (fun i -> Array.copy a.(i)) in
    Hashtbl.add dico kle (!compteur);
    cycle a;
    incr compteur;
  done;
  Printf.printf "%d \n" !compteur;
  let dc = (Hashtbl.find dico a)  in
  let tc = !compteur - dc  in
  let nb_realisation = ((1000000-dc) mod tc) + dc - 2 in
  for i = 0 to nb_realisation -1 do
    cycle debut
  done;
   for i = 0 to lx-1 do
    for j = 0 to ly-1 do 
      if debut.(i).(j) = "O" then
        c := !c + lx - i;
    done;
  done;
  !c






let () = 
  let fichier = read_lines (open_in "day14.txt") in
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
