
let rec read_lines f =
    try
        let line = input_line f in
        line :: read_lines f
    with End_of_file -> []

let rec print_list = 
  function
    [] -> ()
  | e::l -> print_string e ; print_string "\n" ; print_list l

let res tab = 
  let c = ref 0 in 
  let t2 = [|0;1;2;3;4;5;6;7;8;9|] in
  let couple = ref (-1,-1) in
  for i = 0 to Array.length tab -1 do 
    for j = 0 to String.length tab.(i) -1 do
      let nb = int_of_char tab.(i).[j] - 48 in
      for k = 0 to Array.length t2 -1 do
        if nb = t2.(k) && fst (!couple) = -1 then
          couple := (nb,-1);
        done;
    done;
    for j = String.length tab.(i) -1 downto 0 do
      let nb = int_of_char tab.(i).[j] -48 in
      for k = 0 to Array.length t2 - 1 do
        if nb = t2.(k) && snd (!couple) = -1 then
          couple := (fst (!couple) ,nb);
      done;
    done;
    let num = 10*(fst (!couple)) + (snd (!couple)) in
    c := !c + num;
    couple := (-1,-1);
  done;
  !c


let search_motif_min s num = 
  let l = String.length num in
  if String.length s  < l then 100 
  else begin
  let search = ref 100 in
  for i = 0 to String.length s -1 do
  if l + i < String.length s then 
    if String.sub s i l = num && !search = 100 then search := i; 

  done;
  !search 
  end
 
let search_motif_max s num = 
  let l = String.length num in
  if String.length s  < l then -1 
  else begin 
  let search = ref (-1) in
  for i = 0 to String.length s -1 do
    if l + i <= String.length s then 
      if String.sub s i l = num then search := i; 
  done;
  !search 
  end
 

let res2 tab =  
  let c = ref 0 in 
  let t = [|"zero";"one";"two";"three";"four";"five";"six";"seven";"eight";"nine"|] in
  let t2 = [|0;1;2;3;4;5;6;7;8;9|] in
  let couple = ref (-1,-1) in
  for i = 0 to Array.length tab -1 do 
      let min_ind = ref 100 in
      let num_min = ref 1000 in
      let max_ind = ref (-1) in
      let num_max = ref 10000 in
      for k = 0 to Array.length t -1 do
        let trouve_min = search_motif_min tab.(i) t.(k) in
        let trouve_max = search_motif_max tab.(i) t.(k) in
        if trouve_min < !min_ind then begin min_ind := trouve_min; num_min := k; end;
        if trouve_max >= !max_ind && trouve_max != (-1) then begin max_ind := trouve_max; num_max := k;end
    done;
    for j = 0 to String.length tab.(i) -1 do
      let nb = int_of_char tab.(i).[j] - 48 in
        for k = 0 to Array.length t2 -1 do
        if nb = t2.(k) && fst (!couple) = -1  && j <= !min_ind then
          couple := (nb,-1);
        if !min_ind < j  && !min_ind != 100 && fst (!couple) = -1 then 
          couple := (!num_min,-1);
        done;
    done;
    for j = String.length tab.(i) -1 downto 0 do
      let nb = int_of_char tab.(i).[j] - 48 in
      Printf.printf "l'indice est %d \n" !max_ind;
        for k = 0 to Array.length t2 -1 do
        if nb = t2.(k) && snd (!couple) = -1  &&  j >= !max_ind  then begin
            couple := (fst (!couple),nb);
        end;
        if !max_ind > j  && !max_ind != -1 && snd (!couple) = -1 then begin 
          couple := (fst (!couple),!num_max);
        end 
        done;
    done;
    print_int (fst (!couple));
    print_string " ";
    print_int (snd (!couple));
    print_string "\n";
    let num = 10*(fst (!couple)) + (snd (!couple)) in
    c := !c + num;
    couple := (-1,-1);
  done;
  !c   



    

let () = 
  let fichier = read_lines (open_in "day1.txt") in
  let tab =  Array.of_list fichier in
  let r1 = res tab in
  let r2 = res2 tab in
  print_string "\n \n";
  print_string "le résultat est : ";
  print_int (r1);
  print_string "\n \n";
  print_string "le résultat 2 est : ";
  print_int (r2);
