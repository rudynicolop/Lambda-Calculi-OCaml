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
     | "Lam" { LAM }
     | "." { DOT }
     | "fun" { FUN }
     | ":" { COLON }
     | "=>" { MAPSTO }
     | "forall" { FORALL }
     | "," { COMMA }
     | "->" { ARROW }
     | id as v { VAR v }
     | eof { EOF }