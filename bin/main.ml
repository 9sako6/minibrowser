let () =
  let html = Core.In_channel.read_all "test/fixtures/rainbow/index.html" in
  let css = Core.In_channel.read_all "test/fixtures/rainbow/global.css" in
  Minibrowser.render ~html ~css ()
