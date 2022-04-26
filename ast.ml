
(* Définition de types *)

type inputsymb = char list;;
type states = char list;;
type stack = char list;;
type stacksymb = char list;;

(*correspond à l'ordre des déclarations de l'exemple*)
type declaration = inputsymb*stacksymb*states*char*char;;

type trans= char*(char option)*char*char*stack;;
type transitions = trans list;;

type automaton = declaration*transitions;;


(* Fonctions d'affichage *)

let rec list_as_string = function 
  | [] -> ""
  | c::l -> (Char.escaped c)^(list_as_string l)
;;

let rec stack_as_string = function 
  | []-> ""
  | c::l -> (Char.escaped c)^";"^stack_as_string l
;;

let trans_as_string = function
  |(a,None,c,d,s ) -> "("^(Char.escaped a)^", ,"^(Char.escaped c)^","^(Char.escaped d)^","^(stack_as_string s)^")\n"
  |(a,Some(b),c,d,s ) ->"("^(Char.escaped a)^","^(Char.escaped b)^","^(Char.escaped c)^","^(Char.escaped d)^","^(stack_as_string s)^")\n"
;;

(*Remarque : peut etre afficher avec un espace les transitions concernant des
états différents*)
let rec transitions_as_string = function
  |[] -> ""
  |t::l -> (trans_as_string t)^(transitions_as_string l)
;;

let declaration_as_string = function 
(l1, l2, l3, c1, c2) -> 
  "input symbols: "^(list_as_string l1)^"\n"^
  "stack symbols: "^(list_as_string l2 )^"\n"^
  "states: "^(list_as_string l3)^"\n"^
  "initial state: "^(Char.escaped c1)^"\n"^
  "initial stack: "^(Char.escaped c2)^"\n"
;;

let automaton_as_string = function 
  (d, s) -> 
    (declaration_as_string d)^"\n"^
    "transitions: \n\n"^
    (transitions_as_string s)
;;