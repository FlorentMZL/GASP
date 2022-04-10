
(* Définition de types *)

type inputsymb = char list;;
type states = char list;;
type stack = char list;;

type stacksymb = char list;;

type trans= char*(char option)*char*char*stack;;
type transitions = trans list;;
type automaton = inputsymb*states*char*stacksymb*char*transitions


(* Fonctions d'affichage *)

let rec list_as_string = function 
  | [] -> ""
  | c::l -> c^(list_as_string l)
;;

let rec stack_as_string = function 
  | []-> ""
  | c::l -> c^";"^stack_as_string l
;;

let trans_as_string = function
  |(a,None,c,d,s ) -> "("^a^", ,"^c^","^d^","^(stack_as_string s)^")\n"
  |(a,Some(b),c,d,s ) ->"("^a^","^b","^c^","^d^","^(stack_as_string s)^")\n"
;;

(*Remarque : peut etre afficher avec un espace les transitions concernant des
états différents*)
let rec transitions_as_string = function
  |[] -> ""
  |t::l -> (trans_as_string t)^(transitions_as_string l)
;;

let automaton_as_string = function 
  (l1, l2, c1, l3, c2, s) -> 
    "input symbols: "^(list_as_string l1)^"\n"^
    "stack symbols: "^(list_as_string l3 )^"\n"^
    "states: "^(list_as_string l2)^"\n"^
    "initial state: "^c1^"\n"^
    "initial stack: "^c2^"\n\n"^
    "transitions: \n\n"^
    (transitions_as_string s)
;;