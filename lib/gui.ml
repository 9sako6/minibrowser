let draw_rect (Display_command.Rect ((r, g, b), { x; y; width; height }))
    context =
  let x0 = x in
  let x1 = x +. width in
  let y0 = y in
  let y1 = y +. height in
  Cairo.set_source_rgba context 0. 0. 0. 1.;
  Cairo.move_to context x0 y0;
  List.iter
    (fun (x, y) -> Cairo.line_to context x y)
    [ (x1, y0); (x1, y1); (x0, y1) ];
  Cairo.set_source_rgba context r g b 1.;
  Cairo.fill context

let draw _drawing_area ~max_width ~html ~css context =
  let commands = Display_command.build ~max_width ~html ~css in
  List.iter (fun cmd -> draw_rect cmd context) commands;
  true

let render_on_window ~html ~css ~window_width ~window_height () =
  ignore (GMain.init ());
  let window =
    GWindow.window ~title:"minibrowser" ~width:window_width
      ~height:window_height ~resizable:true ()
  in
  ignore (window#connect#destroy ~callback:GMain.quit);

  let d = GMisc.drawing_area ~packing:window#add () in

  ignore
    (d#misc#connect#draw
       ~callback:(draw d ~max_width:(float window_width) ~html ~css));
  window#show ();
  GMain.main ()

let render_on_png ~html ~css ?(window_width = 1600) ?(window_height = 900)
    ~png_file_name () =
  let surf =
    Cairo.Image.create Cairo.Image.ARGB32 ~w:window_width ~h:window_height
  in
  let cr = Cairo.create surf in
  ignore (draw () ~max_width:(float window_width) ~html ~css cr);
  Cairo.PNG.write surf png_file_name

let render ~html ~css ?(window_width = 1600) ?(window_height = 900) () =
  render_on_window ~html ~css ~window_width ~window_height ()
