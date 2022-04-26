let lexbuf = Lexing.from_channel stdin 

let ast = Parser.input Lexer.main lexbuf 

let _ = Printf.printf "Parse:\n%s\n" (Ast.automaton_as_string ast)