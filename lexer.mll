{
open Parser
}

let layout = [ ' ' '\t' '\n' ]
let lettres = [ 'a'-'z''A'-'Z''1'-'9' ]

(*Définition des tokens *)

rule read = parse
  | layout		{ read lexbuf } (*on passe à la suite*)
  | "input symbols:"       { INPUTSYMB }
  | "transitions:"         { TRANS }
  | "stack symbols:"       { STACKSYMB}
  | "states:"              { STATES}
  | "initial state:"       { INITSTATE }
  | "initial stack symbol:"{ INITSTACKSYMB }
  | ')'			                { RPAREN }
  | '('			                { LPAREN }
  | ","		                  { VIRGULE }
  | ";"                     { POINTVIRGULE }
  | lettres as l            { LETTRE (l) }
  | "end"                   { END }
  | eof			                { EOF }
  | _			                  { failwith "unexpected character" }
