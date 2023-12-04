(* Function to extract calibration values from a line *)
let extract_calibration_value line =
  let is_digit c = c >= '0' && c <= '9' in
  let rec find_first_digit index =
    if index < String.length line then
      if is_digit line.[index] then index
      else find_first_digit (index + 1)
    else failwith "No digit found in the line"
  in
  let rec find_last_digit index =
    if index >= 0 then
      if is_digit line.[index] then index
      else find_last_digit (index - 1)
    else failwith "No digit found in the line"
  in
  let first_digit_index = find_first_digit 0 in
  let last_digit_index = find_last_digit (String.length line - 1) in
  int_of_string (String.sub line first_digit_index (last_digit_index - first_digit_index + 1))

(* Function to process each line in the calibration document *)
let process_line line total_sum =
  try
    let calibration_value = extract_calibration_value line in
    total_sum + calibration_value
  with
  | Failure _ -> total_sum (* Ignore lines without valid calibration values *)

(* Function to read the calibration document and calculate the sum *)
let process_calibration_document filename =
  let file = open_in filename in
  let rec process_lines total_sum =
    try
      let line = input_line file in
      let new_sum = process_line line total_sum in
      process_lines new_sum
    with
    | End_of_file -> total_sum
    | Sys_error msg -> close_in file; failwith msg
  in
  let total_sum = process_lines 0 in
  close_in file;
  total_sum

(* Main function *)
let () =
  let filename = "day1.txt" in
  let total_sum = process_calibration_document filename in
  Printf.printf "The sum of all calibration values is: %d\n" total_sum
