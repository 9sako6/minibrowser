let root_path_from_exe = "../../../../"

let actual_path_from_exe file_name =
  root_path_from_exe ^ "/test/visual_regression_test/actual/" ^ file_name

let read_fixture_file path =
  let fixture_path_from_exe path =
    root_path_from_exe ^ "/test/fixtures/" ^ path
  in
  Core.In_channel.read_all (fixture_path_from_exe path)

let compare png_file_name =
  let command =
    Printf.sprintf "cd %s/test/visual_regression_test && npm test -- %s %s %s"
      root_path_from_exe
      ("expected/" ^ png_file_name)
      ("actual/" ^ png_file_name)
      ("diff/" ^ png_file_name)
  in
  Sys.command command

let () =
  let html = read_fixture_file "rainbow/index.html" in
  let css = read_fixture_file "rainbow/global.css" in
  Minibrowser.render_on_png ~html ~css ~window_width:200 ~window_height:200
    ~png_file_name:(actual_path_from_exe "rainbow.png")
    ();

  let status = compare "rainbow.png" in
  assert (status = 0)
