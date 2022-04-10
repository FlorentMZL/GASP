%{
    open Ast
%}

%token RPAREN LPAREN INPUTSYMB STACKSYMB INITSTATE INITSTACKSYMB 
%token TRANS EOF
%token<string> LETTRE

%start<Ast.automaton> input

%%

input: c=automaton EOF {([],[], 'a', [],'c', [])}

automaton: {}