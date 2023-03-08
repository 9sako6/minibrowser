(* let () =
   let html = Minibrowser.Io.read "fixtures/rainbow/index.html" in
   let css = Minibrowser.Io.read "fixtures/rainbow/global.css" in
   Minibrowser.Renderer.render html css *)

let () = Minibrowser.Gui.main ()

(**************************************************************************)
(*    Lablgtk - Examples                                                  *)
(*                                                                        *)
(*    This code is in the public domain.                                  *)
(*    You may freely copy parts of it in your application.                *)
(*                                                                        *)
(**************************************************************************)

(* $Id$ *)

(* let _ = GMain.init ()
   let window = GWindow.window ~border_width:10 ~width:300 ~height:200 ()
   let button = GButton.button ~label:"Hello World" ~packing:window#add ()

   let main () =
     let _ =
       window#event#connect#delete ~callback:(fun _ ->
           prerr_endline "Delete event occured";
           true)
     in
     let _ = window#connect#destroy ~callback:GMain.quit in
     let _ =
       button#connect#clicked ~callback:(fun () -> prerr_endline "Hello World")
     in
     let _ = button#connect#clicked ~callback:window#destroy in
     window#show ();
     GMain.main ()

   let _ = Printexc.print main () *)
