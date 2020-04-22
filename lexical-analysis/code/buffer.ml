exception End_of_buffer

let from_string s =
  let pos = ref 0 in
  let buffer () =
    if !pos = Bytes.length s then
      raise End_of_buffer
    else
    begin
      let c = s.[!pos] in
      pos := !pos + 1;
      c
    end
  in
  buffer

let from_file fname =
  let fin      = open_in fname in
  let line_pos = ref 0 in
  let line     = ref "" in
  let buffer () =
    try
      (* Has read past the end of line *)
      if !line_pos = String.length !line then
      begin
        line_pos := 0;
        line     := (input_line fin);
        let c = (!line).[!line_pos] in
        begin
          line_pos := !line_pos + 1;
          c
        end
      end
      else
      begin
        let c = (!line).[!line_pos] in
        begin
          line_pos := !line_pos + 1;
          c
        end
      end
    with
      End_of_file ->
      begin
        print_endline "EOF!";
        close_in fin;
        raise End_of_buffer
      end
  in
  buffer
