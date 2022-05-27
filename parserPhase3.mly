%{
open Ast
%}


%token STATE NEXT TOP 
%token  CHANGE POP CASE PUSH BEGIN REJECT
%token INPUTSYMB STACKSYMB STATES PROGRAM
%token INITSTATE INITSTACKSYMB 
%token VIRGULE DEUXPOINTS OF 
%token EOF END 
%token<char> LETTRE

%start<Ast.program> input

%%

(*On veut renvoyer un objet de type Ast.program*)
input: c=programF EOF { c } 
(*type programF = declaration*expr *)
programF : 
    d = declarations p = program {(d,p)}


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





program : 
    |PROGRAM DEUXPOINTS i= instructionGlobal {i }

instructionGlobal : 
    |CASE TOP OF c = topCase {TopCase c}
    |CASE NEXT OF c = nextCase{NextCase c}
    |CASE STATE OF c = stateCase{StateCase c}
    |i = instruction {i}

instruction : 
    |BEGIN i = instructionGlobal END {i}
    |POP  {Pop}
    |PUSH l = LETTRE {Push (l)}
    |CHANGE l = LETTRE {Change (l)}
    |REJECT {Reject}
topCase :
    |l = LETTRE DEUXPOINTS i = instruction c = topCase {(l,i)::c}
    |{[]}    

stateCase:
    |l = LETTRE DEUXPOINTS i = instruction c = stateCase {(l,i)::c}
    |{[]}

nextCase : 
    |l = LETTRE DEUXPOINTS i = instruction c = nextCase {(l,i)::c}
    |{[]}