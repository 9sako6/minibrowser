let fixture_dir = "../../../test/fixtures/"

let () =
  let html = Core.In_channel.read_all (fixture_dir ^ "rainbow/index.html") in
  let css = Core.In_channel.read_all (fixture_dir ^ "rainbow/global.css") in
  Minibrowser.render_on_png ~html ~css ~window_width:200 ~window_height:200
    ~png_file_name:"rainbow.png" ()
