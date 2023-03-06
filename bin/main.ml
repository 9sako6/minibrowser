let () =
  let html = Minibrowser.Io.read "fixtures/minimal/example.html" in
  let css = Minibrowser.Io.read "fixtures/minimal/example.css" in
  let board = Minibrowser.Ui.build html css in
  Minibrowser.Ui.render board
