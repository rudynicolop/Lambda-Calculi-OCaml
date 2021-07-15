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
     | "-->" { KARROW }
     | "Bot" { BOT }
     | "Fun" { TFUN }
     | "::" { DOUBLECOLON }
     | "." { DOT }
     | "->" { TARROW }
     | "fun" { FUN }
     | ":" { COLON }
     | "=>" { MAPSTO }
     | id as v { VAR v }
     | eof { EOF }