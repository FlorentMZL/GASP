
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

(* Affichage d'une transition *)
let trans_as_string = function
  |(a,None,c,d,s) -> "("^(Char.escaped a)^",,"^(Char.escaped c)^","^(Char.escaped d)^","^(stack_as_string s)^")\n"
  |(a,Some(b),c,d,s) ->"("^(Char.escaped a)^","^(Char.escaped b)^","^(Char.escaped c)^","^(Char.escaped d)^","^(stack_as_string s)^")\n"
;;

(* Affichage de toutes les transitions *)
let rec transitions_as_string = function
  |[] -> ""
  |t::l -> (trans_as_string t)^(transitions_as_string l)
;;

(* Affichage de la déclaration de l'automate *)
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

(**** Fonction pour les listes ****)

(* fonction pour convertir un mot en liste de char *)
let string_to_char_list s =
  let n = String.length s in
  let rec loop k l =
    if k<0 then l
    else let c = String.get s k in loop (k-1) (c::l)
  in loop (n-1) []
;;

(* Renvoie le dernier élément d'une liste *)
let rec list_last l = 
  match l with 
  |[x]->x
  |h::t -> list_last t
  |[]-> failwith("liste vide")
;;
(**** Fonctions d'évaluation ****)

type word = string list;;
type config = char*stack*word;; (*état, pile, reste entrée*)

(* Mise à jour du haut de la pile *)
let change_stack s n =
    (*on retire le haut de la pile*)
    let s = match s with h::s -> s | [] -> failwith "transition impossible" in
    (*on ajoute les nouveaux éléments en haut de la pile*)
    List.rev_append n s
;;

(* Applique la transition tr à la configuration cf. 
   On a déjà vérifié si les le haut de la pile et le prochain caractère 
   correspondaient*)
let apply_transition cf tr =
  match (cf, tr) with 
  |(_,_,[]),(_, Some _,_,_,_) -> failwith "transition impossible"
  |(_,s,c::w),(_,Some a,_,q,n) ->(q,(change_stack s n), w) 
  |(_,s,w), (_, None,_,q,n)-> (q,(change_stack s n), w) (*epsilon transition*)
;;

let equal_stack_head s h =
  match s with 
  |[] -> false
  |x::s -> x=h
;;

let equal_word_char w c =
  match w with
  |[]-> false
  |x::w -> x=c
;;

(* Trouve la transition à appliquer : on parcours la liste du haut 
   vers le bas et on prend la première transition qui s'applique*)

let rec find_transition cf trs  =
  let (q, s, w) = cf in 
  match trs with 
  |[]-> None
  |(q1, Some a, h, q2, n)::_ when q1 = q && (equal_stack_head s h) 
                                  && (equal_word_char w a)
    ->  Some (q1, Some a, h, q2, n)
  |(q1, None, h,q2, n)::_ when q1 = q && (equal_stack_head s h) 
    -> Some (q1, None, h,q2, n)
  |_::h-> find_transition cf h
;;

(* Fonction de lecture d'un mot dans un automate acceptant par pile vide*)

let rec lecture_mot autom cf = 
  match cf with 
|(q, [], [])  -> print_string("mot accepté"); 
|(q, [], l)-> print_string ("mot refusé. Pile vide mais entrée non vide")
|(q, x, [])-> print_string ("mot refusé. Entrée vide mais pile non vide")
|(q,x,m) -> let (_, tr) = autom in 
  let t' = find_transition (q,x,m) tr in 
  match t' with 
  |None -> print_string("mot refusé. Pas de transition")
  |Some t -> let app = apply_transition (q,x,m) t in 
   lecture_mot autom app
;;


