let build html_string css_string =
  let dom_nodes = html_string |> Dom.Tokenizer.tokenize |> Dom.Parser.parse in
  let css = css_string |> Css.Tokenizer.tokenize |> Css.Parser.parse in
  let style_nodes = dom_nodes |> List.map ref |> List.map (Style.build css) in
  let root = Layout.empty ~width:200. () in
  style_nodes |> List.map (Layout.build ~containing_block:root)

let expose _drawing_area html_string css_string context =
  let nodes = build html_string css_string in
  let rec aux context commands =
    match commands with
    | [] -> true
    | Display_command.((r, g, b), { x; y; width; height }) :: rest ->
        let x0 = x in
        let x1 = x + width in
        let y0 = y in
        let y1 = y + height in
        Cairo.set_source_rgba context 0. 0. 0. 1.;
        Cairo.move_to context (float x0) (float y0);
        List.iter
          (fun (x, y) -> Cairo.line_to context (float x) (float y))
          [ (x1, y0); (x1, y1); (x0, y1) ];

        Cairo.set_source_rgba context
          (float_of_int r /. 255.)
          (float_of_int g /. 255.)
          (float_of_int b /. 255.)
          1.;
        Cairo.fill context;
        aux context rest
  in
  ignore (nodes |> List.map Display_command.build |> List.map (aux context));
  true

let render html_string css_string () =
  ignore (GMain.init ());
  let window =
    GWindow.window ~title:"minibrowser" ~width:800 ~height:400 ~resizable:true
      ()
  in
  ignore (window#connect#destroy ~callback:GMain.quit);

  let d = GMisc.drawing_area ~packing:window#add () in
  ignore (d#misc#connect#draw ~callback:(expose d html_string css_string));

  window#show ();
  GMain.main ()
