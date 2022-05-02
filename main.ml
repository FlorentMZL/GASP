
let usage () = 
  print_string "usage : 
  -parse nomdufichier : teste si l'automate en entrée est correct
  -analyse : teste si le mot est reconnu\n"

;;


  (*Il faudrait factoriser tout ça a l'avenir*)

match Sys.argv with 
|[|_;"-parse"; file|] -> (*Pour accepter un automate a pile*)
  let f = open_in file in 
  let lexbuf =  Lexing.from_channel f in
  let ast = Parser.input (Lexer.read) (lexbuf) in 
  let vA = Ast.verifAutomate ast in let vD = Ast.verifDeterministe ast in 
  if vA && vD  then 
  (*TODO : mettre une explication ?*)
  Printf.printf "Parse:\n%s\n" (Ast.automaton_as_string ast) 
  else if vD then  print_string("Automate non conforme, erreur de symboles")
  else if vA then print_string("Automate non conforme, non déterministe")
  else  print_string("Automate non conforme, erreur de symboles et non déterministe")


|[|_;"-analyse";file;mot|] ->   (*Pour accepter un automate a pile + analyser un mot*)
  let f = open_in file in 
  let lexbuf =  Lexing.from_channel f in
  let ast = Parser.input (Lexer.read) (lexbuf) in 
  let clist = Ast.string_to_char_list mot in (*string_to_list a définir dans Ast*)
  let (d,t) = ast in let (_,_,_,a,b) = d in Ast.lecture_mot (d,t) (a,[b],clist)

  |_ -> usage()
