{
	open Parser
}

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let whitespace = [' ' '\t' '\n']

rule tokenize = parse
     | whitespace { tokenize lexbuf }
     | "(" { LPAREN }
     | ")" { RPAREN }
     | "[" { LBRACK }
     | "]" { RBRACK }
     | "*" { STAR }
     | "=>" { KARROW }
     | "fun" { FUN }
     | "::" { DOUBLECOLON }
     | "." { DOT }
     | "forall" { FORALL }
     | "," { COMMA }
     | "->" { ARROW }
     | "Lam" { LAM }
     | ":" { COLON }
     | id as v { VAR v }
     | eof { EOF }