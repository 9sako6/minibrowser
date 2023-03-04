open Bogue

let render () =
  Widget.label "Hello world" |> Layout.resident |> Bogue.of_layout |> Bogue.run
