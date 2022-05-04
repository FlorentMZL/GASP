
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

(* fonctions auxilliaires *)


let affiche file =
  let ast = get_automaton file in 
  Printf.printf "Parse:\n%s\n" (Ast.automaton_as_string ast) 
;;

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

let main () =
  match Sys.argv with 
  |[|_;"-affiche"; file|]->
    affiche file
  |[|_;"-conforme"; file|]->
    conforme file
  |[|_;"-lit";file;mot|] ->   (*Pour accepter un automate a pile + analyser un mot*)
    lit file mot
  | _ -> usage ()
;;

main ()
