





let usage () = 
  print_string "usage : \n -parse nomdufichier : teste si l'automate en entrée est correct \n -analyse : teste si le mot est reconnu"

;;
let main () =  (* On analyse l'entrée. *)
match Sys.argv with 
|[|_;"-parse";file|] -> (*Pour accepter un automate a pile*)
  let f = open_in file in 
  let lexbuf =  Lexing.from_channel f in
  let ast = Parser.input (Lexer.read) (lexbuf) in 
  (*TODO : mettre une explication ?*)
  Printf.printf "Parse:\n%s\n" (Ast.automaton_as_string ast) 


|[|_;"-analyse";file;mot|] ->   (*Pour accepter un automate a pile + analyser un mot*)
  let f = open_in file in 
  let lexbuf =  Lexing.from_channel f in
  let ast = Parser.input (Lexer.read) (lexbuf) in 
  let clist = Ast.string_to_list mot in (*string_to_list a définir dans Ast*)
  let (d,t) = ast in let (_,_,_,a,b) = d in Ast.lecture_mot (d,t) (a,[b],clist)
|_ -> usage()