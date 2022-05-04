
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

let rec word_as_string = function
  |[]-> ""
  |c::w-> (Char.escaped c)^word_as_string w
;;

let configuration_as_string = function
(q,stack,word) -> 
  "Etat: "^(Char.escaped q)^" Pile: "^(stack_as_string stack)^
  "\nReste: "^(word_as_string word)^"\n"
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


let rec in_list e l=
  match l with
  | [] -> false
  | x::l -> if e=x then true else in_list e l
;;


(* Verification de la bonne formation de l'automate 
   On s'interesse à la partie des déclarations : 
   le symbole de pile initial doit etre parmi les symboles de piles
   de même pour létat initial *)
let verifAutomate aut = 
  let (d,_) = aut in let (_,ssymb, sts, initst, initstack) = d in 
  in_list initstack ssymb && in_list initst sts 
;;

(*TODO  enlever les List.hd : gérer le cas vide *)

(*on verifie si on peut arriver a deux configurations avec 1 transition*)
let rec verifTransiDet t l = 
  let (a,b,c,d,e) = t in match l with 
  |[]->true
  
  (*Si il y a la meme lettre dans les deux transition*)
  |(a1,b1,c1,d1,e1)::q when
  
     (a1 = a && b1 = b && c1 = c && (d != d1 || e != e1)) 
  (*si il n'y a pas de lettre ou alors 1 lettre dans une transition et epsilon dans l'autre transition..*)
  || (a1 = a && ((b1 !=None && b = None)||(b1 = None && b != None)) && c1 = c && (d != d1|| e!=e1))
  (*si il y a 2 epsilon*)
  || (a1 = a && (b1 = None || b = None) && c1 = c)-> false

  (*Sinon on verifie avec l'élement d'après et on rappelle la fonction sur la transition suivante*)
  |(a1,b1,c1,d1,e1)::q -> verifTransiDet t q && verifTransiDet (List.hd q) l  
;;  


let verifDeterministe aut = 
  let (_,t) = aut in let a = List.hd t in verifTransiDet a t 
;;

(**** Fonctions de lecture d'un mot ****)

(* Le sommet de la pile correspond avec le symbole de 
   pile pour effectuer la transition *)
let equal_stack_head stack h =
  match stack with 
  |[] -> false
  |x::s -> x=h
;;

(* On compare la première lettre de l'entrée avec le caractère de 
   la transition *)
let equal_first_letter word c =
 equal_stack_head word c
;;


(* Trouve la transition à appliquer : on parcours la liste du haut 
   vers le bas et on prend la première transition qui s'applique.*)

let rec find_transition cf trs  =
  let (etat, pile, mot) = cf in 
  match trs with 
  (*Aucune transition ne convient*)
  |[]-> None
  (*Transition avec une lettre consommée*)
  |(q1, Some a, h, q2, n)::_ when q1 = etat && (equal_stack_head pile h) 
                                  && (equal_first_letter mot a)
    ->  let t = (q1, Some a, h, q2, n) 
  in print_string (trans_as_string t); Some t
  (*Transition epsilon*)
  |(q1, None, h,q2, n)::_ when q1 = etat && (equal_stack_head pile h) 
    -> let t = (q1, None, h,q2, n)
  in print_string (trans_as_string t); Some t
  |_::h-> find_transition cf h
;;


(* Mise à jour du haut de la pile *)
let change_stack stack n =
  (*on retire le haut de la pile*)
  let s = match stack with h::s -> s | [] -> failwith "transition impossible, la pile est vide" in
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

(* Fonction de lecture d'un mot dans un automate acceptant par pile vide*)

let rec lecture_mot autom cf =
  print_string (configuration_as_string cf);
  match cf with 
|(q, [], [])  -> print_string("mot accepté\n"); 
|(q, [], l)-> print_string ("mot refusé. Pile vide mais entrée non vide\n")

|(q,x,m) -> let (_, tr) = autom in 
  let t' = find_transition (q,x,m) tr in 
  match t' with 
  |None -> if m=[] then print_string 
    ("mot refusé. Entrée vide mais pile non vide\n")
  else print_string("mot refusé. Pas de transition\n")
  |Some t -> let app = apply_transition (q,x,m) t in 
   lecture_mot autom app
;;


