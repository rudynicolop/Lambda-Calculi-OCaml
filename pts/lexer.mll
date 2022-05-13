{
	open Parser
}

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let whitespace = [' ' '\t' '\n']

rule tokenize = parse
     | whitespace { tokenize lexbuf }
     | "(" { LPAREN }
     | ")" { RPAREN }
     | "*" { STAR }
     | "[]" { SQUARE }
     | "fun" { FUN }
     | "∏" { PI }
     | ":" { COLON }
     | "." { DOT }
     | id as v { VAR v }
     | eof { EOF }