let lexbuf = Lexing.from_channel stdin 

let ast = Parser.input Lexer.read lexbuf 

(*TODO : mettre une explication ?*)

let _ = Printf.printf "Parse:\n%s\n" (Ast.automaton_as_string ast) 