%{
    open Ast
%}

%token RPAREN LPAREN INPUTSYMB STACKSYMB STATES INITSTATE INITSTACKSYMB VIRGULE POINTVIRGULE
%token TRANS EOF
%token<char> LETTRE

%start<Ast.automaton> input

%%

input: c=automaton EOF {([],[], 'a', [],'c', [])}

automaton: d = declaration t = transitions  { d t }

declaration : i = inputsymbols s = stacksymbol st = states ini = initialstate init = initialstack
            { i s st ini init }


inputsymbols: 
    INPUTSYMB s = suitelettres  {s}

stacksymbol: 
    STACKSYMB s = suitelettres {s}

states: 
    STATES s = suitelettres{s}

initialstate: 
    INITSTATE l = LETTRE {l}

initialstack: 
    INITSTACKSYMB l = LETTRE {l}

suitelettres:
    l = LETTRE {[l]}
    |l = LETTRE VIRGULE s = suitelettres {[l]@s } (* je crée une liste de char mais c'est peut etre pas bon*)


transitions: 
    TRANS t = translist {t}

translist: {} (*a refaire avec le epsilon*)
    | t = transition tr = translist {t tr}




transition: LPAREN l1 = LETTRE VIRGULE l2 = lettreouvide VIRGULE l3 =LETTRE VIRGULE l4 = LETTRE VIRGULE 
 s1 = stack RPAREN                    {} (*Je sais pas quoi mettre là dedans*)



lettreouvide: {}(* a refaire avec le epsilon*)
            | l = LETTRE {l}


stack: {}(*de même*)
    | n = nonemptystack {n}



nonemptystack : l = LETTRE {[l]}
              | l = LETTRE POINTVIRGULE n = nonemptystack {[l]@n} (*aucune idée de si ça pourrait marcher non plus*)





    