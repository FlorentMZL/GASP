{
open Parser
}

let layout = [ ' ' '\t' '\n' ]
let lettres = [ 'a'-'z''A'-'Z''1'-'9' ]

(*Définition des tokens *)

rule main = parse
  | layout		{ main lexbuf } (*?*)
  | "input symbols :"       { INPUTSYMB }
  | "transitions :"         { TRANS }
  | "stack symbols :"       { STACKSYMB}
  | "states :"              { STATES}
  | "initial state :"       { INITSTATE }
  | "initial stack :"       { INITSTACKSYMB }
  | ')'			                { RPAREN }
  | '('			                { LPAREN }
  | ","		                  { VIRGULE }
  | ";"                     { POINTVIRGULE }
  | lettres as l            { LETTRE (l) }
  | eof			                { EOF }
  | _			                  { failwith "unexpected character" }
