{
open ParserPhase3
}

let layout = [ ' ' '\t' '\n' ]
let lettres = [ 'a'-'z''A'-'Z''1'-'9' ]

(*Définition des tokens *)

rule read = parse
  | layout		{ read lexbuf } (*on passe à la suite*)
  | "input symbols:"       { INPUTSYMB }
  | "stack symbols:"       { STACKSYMB}
  | "states:"              { STATES}
  | "initial state:"       { INITSTATE }
  | "initial stack symbol:"{ INITSTACKSYMB }
  | "begin"                 { BEGIN }
  | "program"             { PROGRAM }
  | "state"                 { STATE }
  | "push"                  { PUSH }
  | "change"                { CHANGE}
  | "pop"                   { POP }
  | "top"                   { TOP }
  | "case"                  { CASE }
  | "next"                  { NEXT }
  | "of"                    { OF }
  | ","		                  { VIRGULE }
  | ":"                     { DEUXPOINTS }
  |"reject"                 {REJECT}
  |"end"                   { END }
  | lettres as l            { LETTRE (l) } 
  | eof			                { EOF }
  | _			                  { failwith "unexpected character" }
