let sum l =
  let rec loop acc = function
      [] -> acc
    | h :: t -> loop (acc + h) t
  in
  loop 0 l
