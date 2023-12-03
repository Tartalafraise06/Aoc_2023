
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

let print_array tab = 
  for i = 0 to Array.length tab -1 do 
    print_string tab.(i);
    print_string "\n"
  done

let is_in i s1 s2 s3 = 
  let longueur_s = String.length s2 -1 in
  let symb = [|'+';'*';'$';'&';'#';'/';'@';'=';'%';'-'|] in
  let flag = ref false in
  String.iter (fun a -> if Array.mem a symb then flag := true;) s1;
  String.iter (fun a -> if Array.mem a symb then flag := true;) s3;
  if  Array.mem s2.[0] symb then flag := true; 
  if Array.mem s2.[longueur_s] symb then flag := true;
  !flag



let res tab = 
  let c = ref 0 in
  let taille = String.length tab.(0) in
  for i = 0 to Array.length tab -1 do 
    let j = ref 0 in
    while !j <= String.length tab.(i) -1 do
      if is_digit tab.(i).[!j]  then begin 
        let compteur = ref 0 in
        while  !j+(!compteur) < taille && is_digit tab.(i).[!compteur + (!j)]  do
          incr compteur;
        done; 
        Printf.printf "ligne  :  %d \n" i;
(*        print_string "deb \n";*)
        let deb = (if !j = 0 then (!j) else !j-1) in
        let fin = (if !compteur + (!j) < taille then !compteur + (!j) +1  else  !compteur + (!j) ) in
        let s1 = if i = 0 then "" else (String.sub tab.(i-1) deb (fin-deb)) in
 (*       print_string "mid \n";*)
        let s2 =  (String.sub tab.(i) deb (fin-deb)) in
(*        print_string "mid \n";*)
        let s3 = if i = Array.length tab -1 then "" else (String.sub tab.(i+1) deb (fin-deb)) in
(*        print_string "fin \n";*)
        if is_in j s1 s2 s3 then begin
          let number = (int_of_string (String.sub tab.(i) (!j) (!compteur))) in
          c:=  number + (!c);
          Printf.printf "%d  \n" number;
        end;
        j := !j  + (!compteur);
      end;
  
      incr j; 
    done; 
  done;
!c

let is_in2 i s1 s2 s3 = 
  let longueur_s = String.length s2 -1 in
  let symb = [|'*'|] in
  let flag = ref 0 in
  let num = ref 0 in
  let j = ref 0 in 
  let l = ref 0 in
  String.iter (fun a -> incr num;  if Array.mem a symb then begin flag := 1; j := !num; l:= 1; end) s1;
  num:= 0;
  String.iter (fun a -> incr num;  if Array.mem a symb then begin flag := 1; j := !num; l:= 3; end) s3;
  if  Array.mem s2.[0] symb then begin flag := 1; j := 1; l := 2; end;
  if Array.mem s2.[longueur_s] symb then begin flag := 1; j := longueur_s+1;l:= 2; end;
  [|!flag;!j;!l|]

let res2 tab = 
  let c = ref 0 in
  let taille = String.length tab.(0) in
  let l = ref [] in
  let id = ref 0 in
  for i = 0 to Array.length tab -1 do 
    let j = ref 0 in
    while !j <= String.length tab.(i) -1 do
      if is_digit tab.(i).[!j]  then begin 
        let compteur = ref 0 in
        while  !j+(!compteur) < taille && is_digit tab.(i).[!compteur + (!j)]  do
          incr compteur;
        done; 
        Printf.printf "ligne  :  %d \n" i;
(*        print_string "deb \n";*)
        let deb = (if !j = 0 then (!j) else !j-1) in
        let fin = (if !compteur + (!j) < taille then !compteur + (!j) +1  else  !compteur + (!j) ) in
        let s1 = if i = 0 then "" else (String.sub tab.(i-1) deb (fin-deb)) in
 (*       print_string "mid \n";*)
        let s2 =  (String.sub tab.(i) deb (fin-deb)) in
(*        print_string "mid \n";*)
        let s3 = if i = Array.length tab -1 then "" else (String.sub tab.(i+1) deb (fin-deb)) in
(*        print_string "fin \n";*)
        let t = is_in2 j s1 s2 s3 in
        if t.(0) = 1 then begin
          incr id;
          let number = (int_of_string (String.sub tab.(i) (!j) (!compteur))) in
          let i_add,j_add = t.(2)-2+i , deb + t.(1) in
          l:= [|number;i_add;j_add;!id|]::(!l); 
          Printf.printf "%d  \n" number;
        end;
        j := !j  + (!compteur);
      end;
      incr j; 
    done; 
  done;
  List.iter (fun a -> (List.iter (fun l_parcourt -> if a.(3) <> l_parcourt.(3) && a.(1) = l_parcourt.(1) && a.(2) = l_parcourt.(2) then c:= !c + a.(0)*l_parcourt.(0)) !l)) !l;
  (!c)/2



let () = 
  let fichier = read_lines (open_in "day3.txt") in
  let tab =  Array.of_list fichier in
  let r1 = res tab in 
  let r2 = res2 tab in 
  print_string "\n \n";
  print_string "le résultat est : ";
  print_int (r1);
  print_string "\n \n";
  print_string "le résultat 2 est : ";
  print_int (r2);
