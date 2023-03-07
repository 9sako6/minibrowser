let () =
  let html = Minibrowser.Io.read "fixtures/rainbow/index.html" in
  let css = Minibrowser.Io.read "fixtures/rainbow/global.css" in
  Minibrowser.Renderer.render html css
