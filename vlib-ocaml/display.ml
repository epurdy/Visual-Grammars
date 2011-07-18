let graphics_data_of_image img f =
  let width = Image.width img in
  let height = Image.height img in 
  let data = Array.make_matrix height width 0 in
    for y = 0 to height-1 do
      for x = 0 to width-1 do
	data.(y).(x) <- f (Image.get img (x, y))
      done
    done;
    data
    
let data_of_gray (v : int) =
  (v lsl 16) lor (v lsl 8) lor (v)
  
let data_of_color ((r,g,b) : Image.rgb) =
  (r lsl 16) lor (g lsl 8) lor (b)

let open_graph w h =
  let geom = (string_of_int w) ^ "x" ^ (string_of_int h) ^ "+0+0" in
    Graphics.open_graph (" " ^ geom)

let close_graph () = 
  Graphics.close_graph ()  

let show_image dspdata =
  Graphics.draw_image (Graphics.make_image dspdata) 0 0
    
let show_gray img = 
  let dspimg = graphics_data_of_image img data_of_gray in
    show_image dspimg 

let show_color img = 
  let dspimg = graphics_data_of_image img data_of_color in
    show_image dspimg 

let wait_mouse_down () =
  let status = Graphics.wait_next_event [Graphics.Button_down] in 
  let size_y = (Graphics.size_y ()) - 1 in
    (status.Graphics.mouse_x, size_y - status.Graphics.mouse_y)

let wait_mouse_up () =
  let status = Graphics.wait_next_event [Graphics.Button_up] in
  let size_y = (Graphics.size_y ()) - 1 in
    (status.Graphics.mouse_x, size_y - status.Graphics.mouse_y)
