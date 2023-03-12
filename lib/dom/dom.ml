module Node = Node

let parse html = html |> Tokenizer.tokenize |> Parser.parse
