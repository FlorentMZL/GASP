
(**** Définition de types ****)

type inputsymb = char list;;
type states = char list;;
type stack = char list;;
type stacksymb = char list;;

type declaration = inputsymb*stacksymb*states*char*char;;
(*correspond à l'ordre des déclarations de l'exemple*)

type trans= char*(char option)*char*char*stack;;
type transitions = trans list;;

type automaton = declaration*transitions;;


(**** Fonctions d'affichage ****)

(* [a;b;c] est affiché comme : a, b, c *)
let rec list_as_string = function 
  | [] -> ""
  | [c] -> (Char.escaped c)
  | c::l -> (Char.escaped c)^", "^(list_as_string l)
;;

(* [A;B;C] est affiché comme : A;B;C *)
let rec stack_as_string = function 
  | [] -> ""
  | [c] -> (Char.escaped c)
  | c::l -> (Char.escaped c)^";"^(stack_as_string l)
;;

let trans_as_string = function
  |(a,None,c,d,s) -> "("^(Char.escaped a)^",,"^(Char.escaped c)^","^(Char.escaped d)^","^(stack_as_string s)^")\n"
  |(a,Some(b),c,d,s) ->"("^(Char.escaped a)^","^(Char.escaped b)^","^(Char.escaped c)^","^(Char.escaped d)^","^(stack_as_string s)^")\n"
;;

let rec transitions_as_string = function
  |[] -> ""
  |t::l -> (trans_as_string t)^(transitions_as_string l)
;;

let declaration_as_string = function 
(l1, l2, l3, c1, c2) -> 
  "input symbols: "^(list_as_string l1)^"\n"^
  "stack symbols: "^(list_as_string l2)^"\n"^
  "states: "^(list_as_string l3)^"\n"^
  "initial state: "^(Char.escaped c1)^"\n"^
  "initial stack symbol: "^(Char.escaped c2)^"\n"
;;

(*Fonction générale d'affichage d'un automate*)
let automaton_as_string = function 
  (d, s) -> 
    (declaration_as_string d)^"\n"^
    "transitions: \n\n"^
    (transitions_as_string s)
;;

(*Fonction pour les listes*)

(* il faut une  fonction pour convertir un mot en liste de char*)


let rec list_last l = 
  match l with 
  |[x]->x
  |h::t -> list_last t
  |[]-> failwith("liste vide")
(**** Fonctions d'évaluation ****)

type word = char list;;

type config = char*stack*word;;


(*TODO : il faut peut etre faire les tables d'action et 
   de transitions pour savoir quoi faire? 
   finalement pas : on regarde la premiere lettre 
   et le haut de la pile pour décider*)

(*TODO :
    il faut peut etre mettre le mot dans une liste au départ?
    String.split_on_char '' lemot
    *)


(* Applique la transition tr à la configuration cf *)
let apply_transition cf tr =
  match (cf, tr) with 
  |(q,l1,m),(q1,Some a,x,q2,alph) ->(q2,(List.hd l1)::alph, List.tl m) 
  |(q,l1,m), (q1, None, x, q2, alph)-> (q2, (List.hd l1)::alph, m)
;;

(* Trouve la transition à appliquer : on parcours la liste du haut 
   vers le bas et on prend la première transition qui s'applique*)
let rec find_transition cf trs  =
  let (q, s, w) = cf in 
  match trs with 
  |[]-> failwith("Pas de transition") (*il faudrait peut etre  gérer ça autrement *)
  |(q1, Some a, x, q2, alpha)::h when q1 = q && list_last s = x ->  List.hd trs
  |(q1, None, x,q2, alpha)::h when q1 = q && list_last s = x -> List.hd trs
  |_::h-> find_transition cf h
;;

let rec lecture_mot autom cf = 

  match cf with 
|(q, [], [])  -> print_string("mot accepté"); 
|(q, [], l)-> print_string ("mot refusé. Pile vide mais entrée non vide")
|(q, x, [])-> print_string ("mot refusé. Entrée vide mais pile non vide")
|(q,x,m) -> let (_, tr) = autom in let t = find_transition (q,x,m) tr in let app = apply_transition (q,x,m) t in 
lecture_mot autom app
  


;;


