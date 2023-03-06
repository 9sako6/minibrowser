let () =
  let html = Minibrowser.Io.read "fixtures/minimal/example.html" in
  let css = Minibrowser.Io.read "fixtures/minimal/example.css" in
  Minibrowser.Renderer.render html css
