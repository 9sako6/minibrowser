let () =
  let html = "<div class=\"container\"><p>alice</p><p>bob</p></div>" in
  let css = ".container {display: block; width: 200px; height: 100px;}" in
  let board = Minibrowser.Ui.build html css in
  Minibrowser.Ui.render board
