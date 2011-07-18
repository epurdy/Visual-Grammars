exception Bad_file
    
let rec input_word chan = 
  let rec iter chan s = 
    let c = input_char chan in 
      if c = '\n' || c = ' ' || c = '\t' || c = '\r' then s
      else iter chan (s^(Char.escaped c)) in
  let c = input_char chan in
    if (c = '\n' || c = ' ' || c = '\t' || c = '\r') then
      input_word chan
    else if c != '#' then 
      iter chan (Char.escaped c)
    else (ignore(input_line chan); input_word chan)

let get_header chan = 
  let magic_num = input_word chan in
  let width = int_of_string(input_word chan) in
  let height = int_of_string(input_word chan) in
    (* ignore the maxval for pgms and ppms *)
    (match magic_num with
	 "P4" -> ()
       | "P5" -> ignore(input_word chan)
       | "P6" -> ignore(input_word chan)
       | _ -> raise Bad_file);
    (magic_num, width, height)
     
let write_header chan magic_num width height = 
  output_string chan (magic_num ^ "\n");
  output_string chan 
    ((string_of_int width) ^ " " ^ (string_of_int height) ^ "\n");
  match magic_num with
      "P4" -> ()
    | "P5" -> output_string chan ((string_of_int 255) ^ "\n")
    | "P6" -> output_string chan ((string_of_int 255) ^ "\n")
    | _ -> raise (Invalid_argument "magic_num must be P4, P5 or P6")

let load_pgm name = 
  let chan = open_in_bin name in
  let (magic_num, width, height) = get_header chan in
  let img = Image.create width height 0 in
  let read () = 
    for y = 0 to height-1 do
      for x = 0 to width-1 do
	Image.set img (input_byte chan) (x,y) 
      done
    done 
  in
    if magic_num <> "P5" then raise Bad_file; 
    read (); 
    close_in chan; 
    img

let save_pgm img name = 
  let chan = open_out_bin name in
  let write () = 
    for y = 0 to (Image.height img)-1 do
      for x = 0 to (Image.width img)-1 do
	output_byte chan (Image.get img (x,y))
      done
    done 
  in
    write_header chan "P5" (Image.width img) (Image.height img);
    write (); 
    close_out chan

let load_ppm name = 
  let chan = open_in_bin name in
  let (magic_num, width, height) = get_header chan in
  let img = Image.create width height (0,0,0) in
  let read () = 
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
	let r = (input_byte chan) in
	let g = (input_byte chan) in
	let b = (input_byte chan) in
	  Image.set img (r,g,b) (x,y) 
      done
    done 
  in
    if magic_num <> "P6" then raise Bad_file;
    read (); 
    close_in chan; 
    img

let save_ppm img name = 
  let chan = open_out_bin name in
  let write () = 
    for y = 0 to (Image.height img)-1 do
      for x = 0 to (Image.width img)-1 do
	let (r,g,b) = Image.get img (x,y) in
	  (output_byte chan r;
	   output_byte chan g;
	   output_byte chan b)
      done
    done 
  in
    write_header chan "P6" (Image.width img) (Image.height img);
    write (); 
    close_out chan
