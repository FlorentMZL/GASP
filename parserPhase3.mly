%{
open Ast
%}


%token<string> STATE NEXT TOP 
%token <string> CHANGE POP CASE PUSH 
%token INPUTSYMB STACKSYMB STATES 
%token INITSTATE INITSTACKSYMB 
%token VIRGULE DEUXPOINTS OF 
%token TRANS 
%token EOF END BEGIN
%token<char> LETTRE

%start<Ast.automaton> input

%%

(*On veut renvoyer un objet de type automate*)
input: c=automaton END EOF { c } 

(* type automaton = declaration*transitions *)
automaton: 
    d=declarations t=transitions  { (d, t) }

(*type declaration = inputsymb*stacksymb*states*char*char *)
declarations : 
    i=inputsymbols s=stacksymbol st=states 
    ini=initialstate init=initialstack
                                  { (i, s, st, ini, init) }

inputsymbols: 
    INPUTSYMB s=suitelettresnonvide      { s }

stacksymbol: 
    STACKSYMB s=suitelettresnonvide      { s }

states: 
    STATES s=suitelettresnonvide         { s }

initialstate: 
    INITSTATE l=LETTRE               { l }

initialstack: 
    INITSTACKSYMB l=LETTRE           { l }

suitelettresnonvide:
    |l=LETTRE                               { [l] }
    |l=LETTRE VIRGULE s=suitelettresnonvide { l::s } (* je crée une liste de char mais c'est peut etre pas bon*)

(*type program = case ?*)
program : 
    |c = case {c}

case : 
    |CASE i = situation OF l = instructionListe END { (i,l) } 

instructionListe : 
    |le=LETTRE DEUXPOINTS i = instruction s = instructionListe {(le, i)::s} 
    | (*mettre un truc pour dire qu'il y a plus rien a ajouter dans la liste*)

instruction : 
    |BEGIN l= case {l}(*?*) 
    |p = operation  l = LETTRE {(p,l)}
    


situation : 
    |s = STATE {s}
    |s =NEXT {s}
    |s =TOP {s}

 operation : 
    |p = PUSH {p}
    |p = CHANGE {p}
    |p = POP {p}