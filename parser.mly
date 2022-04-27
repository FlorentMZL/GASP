%{
open Ast
%}

%token RPAREN LPAREN I
%token INPUTSYMB STACKSYMB STATES 
%token INITSTATE INITSTACKSYMB 
%token VIRGULE POINTVIRGULE
%token TRANS 
%token EOF END
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

transitions: 
    TRANS t = translist              { t }

translist:  (* renvoyer une liste de transitions *)
    |                                { [] }
    | t=transition tr=translist      { t::tr }

transition: 
    LPAREN l1=LETTRE VIRGULE 
    l2=lettreouvide VIRGULE 
    l3=LETTRE VIRGULE 
    l4=LETTRE VIRGULE 
    s1=stack RPAREN                { (l1,l2,l3,l4,s1) } 

lettreouvide: 
    |               { None }
    | l=LETTRE      { Some(l)}

stack: 
    |                 { [] }
    | n=nonemptystack { n }

nonemptystack : 
    | l=LETTRE {[l]}
    | l=LETTRE POINTVIRGULE n=nonemptystack {l::n} 





    