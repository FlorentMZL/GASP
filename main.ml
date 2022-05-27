
let usage () = 
  print_string "usage : 
  -affiche MonAutomate : ré-affiche l'automate tel qu'il a été analysé
  -conforme MonAutomate : vérification de la bonne formation de l'automate
  -lit MonAutomate MonMot : teste si le mot est reconnu\n"
;;


let get_automaton file =
  let f = open_in file in 
  let lexbuf =  Lexing.from_channel f in
  Parser.input (Lexer.read) (lexbuf) 
;;
let get_program file = 
  let f = open_in file in 
  let lexbuf = Lexing.from_channel f in 
  ParserPhase3.input (LexerPhase3.read) (lexbuf)

;;


(* fonctions auxilliaires *)


let affiche1 file =
  let ast = get_automaton file in 
  Printf.printf "Parse:\n%s\n" (Ast.automaton_as_string ast) 
;;
let affiche3 file = 
  let prg = get_program file in 
  Printf.printf"Parse : \n%s\n" (Ast.prg_as_string prg)
let afficheProg file = 
  let prg = get_program file in 
  Printf.printf "Parse :\n%s\n" (Ast.prg_as_string prg)





let conforme file =
  let ast = get_automaton file  in 
  let vA = Ast.verifAutomate ast in let vD = Ast.verifDeterministe ast in 
  if vA && vD  then 
    (*TODO : mettre une explication ?*)
  Printf.printf "Parse:\n%s\n" (Ast.automaton_as_string ast) 
  else if vD then  print_string("Automate non conforme, erreur de symboles\n")
  else if vA then print_string("Automate non conforme, non déterministe\n")
  else  print_string("Automate non conforme, erreur de symboles et non déterministe\n")
;;

let lit file mot =
  let ast = get_automaton file in
  let clist = Ast.string_to_char_list mot in (*string_to_list a définir dans Ast*)
  let (d,t) = ast in let (_,_,_,a,b) = d in Ast.lecture_mot (d,t) (a,[b],clist)
;;

let lit3 file mot = 
  let prog = get_program file in 
  let clist = Ast.string_to_char_list mot in  Ast.lecture_mot_prog prog clist

let main () =
  match Sys.argv with 
  |[|_;"-affiche1"; file|]->
    affiche1 file
  |[|_;"-affiche3"; file|]-> affiche3 file
  |[|_;"-conforme"; file|]->
    conforme file
  |[|_;"-lit";file;mot|] ->   (*Pour accepter un automate a pile + analyser un mot*)
    lit file mot

  |[|_;"-lit3";file;mot|] ->   (*Pour accepter un automate a pile + analyser un mot*)
    lit3 file mot  
  | _ -> usage ()
;;

main ()
