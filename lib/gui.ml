(* let render_rect_command command =
   match command with
   | Display_command.t(color, {x; y; width; height})
     -> *)
open Cairo

let polygon = [ (0, 0); (48, 0); (48, 48); (0, 48) ]
let polygon2 = [ (12, 12); (36, 12); (36, 36); (12, 36) ]

let draw_polygon cr = function
  | [] -> ()
  | (x, y) :: tl ->
      set_source_rgba cr 0. 0. 0. 1.;
      move_to cr (float x) (float y);
      List.iter (fun (x, y) -> line_to cr (float x) (float y)) tl;
      set_source_rgba cr (Random.float 1.) (Random.float 1.) (Random.float 1.)
        0.5;
      fill cr

let build html_string css_string =
  let dom_nodes = html_string |> Dom.Tokenizer.tokenize |> Dom.Parser.parse in
  let css = css_string |> Css.Tokenizer.tokenize |> Css.Parser.parse in
  let style_nodes =
    dom_nodes |> List.map ref |> List.map (Style_tree.Node.build css)
  in
  style_nodes |> List.map Layout_box.Block.build

let rec render cr commands =
  match commands with
  | [] -> true
  | Display_command.((r, g, b), { x; y; width; height }) :: rest ->
      print_endline (string_of_int height);
      let x0 = x in
      let x1 = x + width in
      let y0 = y in
      let y1 = y + height in
      set_source_rgba cr 0. 0. 0. 1.;
      move_to cr (float x0) (float y0);
      List.iter
        (fun (x, y) -> line_to cr (float x) (float y))
        [ (x1, y0); (x1, y1); (x0, y1) ];
      List.iter
        (fun (x, y) -> print_endline (Printf.sprintf "(%d, %d)" x y))
        [ (x1, y0); (x1, y1); (x0, y1) ];

      (* set_source_rgba cr (Random.float 1.) (Random.float 1.) (Random.float 1.) 1.;*)
      set_source_rgba cr ((float_of_int r) /. 255.) ((float_of_int g) /. 255.) ((float_of_int b) /. 255.) 1.;
      fill cr;
      render cr rest

let expose _drawing_area cr =
  let html = Io.read "fixtures/rainbow/index.html" in
  let css = Io.read "fixtures/rainbow/global.css" in
  let nodes = build html css in
  let node = nodes |> List.tl |> List.hd in
  let commands = Display_command.build node in
  let _ = render cr commands in

  true

(* let expose _drawing_area cr =
   draw_polygon cr polygon;
   draw_polygon cr polygon2;
   true *)

let main () =
  let _ = GMain.init () in
  let w = GWindow.window ~title:"Drawing demo" ~width:500 ~height:400 () in
  ignore (w#connect#destroy ~callback:GMain.quit);

  let d = GMisc.drawing_area ~packing:w#add () in
  ignore (d#misc#connect#draw ~callback:(expose d));

  w#show ();
  GMain.main ()
