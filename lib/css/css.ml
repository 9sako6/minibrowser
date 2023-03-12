type t = Node.stylesheet

module Node = Node
module Value_map = Value_map
module Value = Value

let parse css = css |> Tokenizer.tokenize |> Parser.parse
