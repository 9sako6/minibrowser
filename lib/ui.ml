open Bogue

let main () =
  let b = Widget.check_box () in
  let l = Widget.label "Hello world" in
  let style = Style.Solid (Draw.black |> Draw.opaque) |> Style.of_bg in
  let box = Widget.box ~w:200 ~h:100 ~style () in
  let layout = Layout.flat_of_w [ b; l; box ] in

  let board = Bogue.of_layout layout in
  Bogue.run board

let render () =
  main ();
  Bogue.quit ()
