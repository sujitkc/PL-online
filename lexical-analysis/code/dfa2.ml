let list_of_string s =
  let rec iter i s =
    if i = (String.length s) then []
    else s.[i] :: (iter (i + 1)) s
  in
  iter 0 s

let one_zero lst =
  let rec init l =
    match l with
      [] -> false
    | h :: t -> if h = '1' then (accept t) else false

  and accept l =
    match l with
      [] -> true
    | h :: t ->
      if h = '0' then (init t)
      else if h = '1'  then (accept t)
      else false
  in
  init (list_of_string lst)

let test_inputs = [
  "1011";
  "010";
]

let main () =
  let rec loop = function
    [] -> ()
  | h :: t ->
      let result = one_zero h in
      begin
        (Printf.printf "%s : %b\n" h result);
        (loop t)
      end
  in
  loop test_inputs

let _ = main ()
    

















let id s =
  let rec one l =
    match l with
      [] -> false
    | h :: t ->
      if (h >= 'A' && h <= 'Z') || (h >= 'a' && h <= 'z') then
        (two t)
      else
        false

  and two l =
    match l with
      [] -> true
    | h :: t ->
      if (h >= 'A' && h <= 'Z') || (h >= 'a' && h <= 'z') || (h >= '0' && h <= '9') then
        (two t)
      else
        false
  in
  one (list_of_string s)
