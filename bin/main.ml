let () =
  let html = Minibrowser.Io.read "test/fixtures/rainbow/index.html" in
  let css = Minibrowser.Io.read "test/fixtures/rainbow/global.css" in
  Minibrowser.Gui.render html css ()
