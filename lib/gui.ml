let build html_string css_string =
  let dom_nodes = html_string |> Dom.Tokenizer.tokenize |> Dom.Parser.parse in
  let css = css_string |> Css.Tokenizer.tokenize |> Css.Parser.parse in
  let style_nodes =
    dom_nodes |> List.map ref |> List.map (Style.build css)
  in
  let root = Layout.empty ~width:200. () in
  style_nodes |> List.map (Layout.build ~containing_block:root)

let rec _render cr commands =
  match commands with
  | [] -> true
  | Display_command.((r, g, b), { x; y; width; height }) :: rest ->
      print_endline (string_of_int height);
      let x0 = x in
      let x1 = x + width in
      let y0 = y in
      let y1 = y + height in
      Cairo.set_source_rgba cr 0. 0. 0. 1.;
      Cairo.move_to cr (float x0) (float y0);
      List.iter
        (fun (x, y) -> Cairo.line_to cr (float x) (float y))
        [ (x1, y0); (x1, y1); (x0, y1) ];
      List.iter
        (fun (x, y) -> print_endline (Printf.sprintf "(%d, %d)" x y))
        [ (x1, y0); (x1, y1); (x0, y1) ];

      Cairo.set_source_rgba cr
        (float_of_int r /. 255.)
        (float_of_int g /. 255.)
        (float_of_int b /. 255.)
        1.;
      Cairo.fill cr;
      _render cr rest

let expose _drawing_area html_string css_string cr =
  let nodes = build html_string css_string in
  let _ = nodes |> List.map Display_command.build |> List.map (_render cr) in
  true

let render html_string css_string () =
  let _ = GMain.init () in
  let window =
    GWindow.window ~title:"minibrowser" ~width:800 ~height:400 ~resizable:true
      ()
  in
  ignore (window#connect#destroy ~callback:GMain.quit);

  let d = GMisc.drawing_area ~packing:window#add () in
  ignore (d#misc#connect#draw ~callback:(expose d html_string css_string));

  window#show ();
  GMain.main ()
