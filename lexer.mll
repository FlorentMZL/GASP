{
open Parser
}

let lettre = ['0'-'9''a'-'z''A'-'Z']

rule main = parse {
    | lettre as l                   { LETTRE (l)}
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | "input symbols:"              { INPUTSYMB }
    | "stack symbols:"              { STACKSYMB }
    | "initial state:"              { INITSTATE }  
    | "initial stack symbol:"       { INITSTACKSYMB }
    | "transitions:"                { TRANS }
    | eof                           { EOF }
    | _                { failwith "unexpected character" }
}