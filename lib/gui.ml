let draw_rect rect context =
  match rect with
  | Display_command.Rect ((r, g, b), { x; y; width; height }) ->
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
  let rec aux context commands =
    match commands with
    | [] -> true
    | (Display_command.Rect _ as rect) :: rest ->
        draw_rect rect context;
        aux context rest
  in
  ignore (aux context commands);
  true

let render ~html ~css () =
  ignore (GMain.init ());
  let window =
    GWindow.window ~title:"minibrowser" ~width:800 ~height:400 ~resizable:true
      ()
  in
  ignore (window#connect#destroy ~callback:GMain.quit);

  let d = GMisc.drawing_area ~packing:window#add () in
  ignore (d#misc#connect#draw ~callback:(draw d ~max_width:200. ~html ~css));

  window#show ();
  GMain.main ()
