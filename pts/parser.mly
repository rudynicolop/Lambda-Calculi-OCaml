%{
    open Syntax
    open Triple
%}

(** Tokens. *)
%token LPAREN RPAREN EOF
%token STAR SQUARE
%token FUN PI COLON DOT
%token <string> VAR

(** Start symbol. *)
%start <cube_sort p_term> prog

(** Type annotations. *)
%type <cube_sort p_term> pabs
%type <cube_sort p_term> papp
%type <cube_sort p_term> pvar

%%

prog:
  | t=pabs EOF { t }

pabs:
  | FUN x=VAR COLON t1=pabs DOT t2=pabs { abs x t1 t2 }
  | PI  x=VAR COLON t1=pabs DOT t2=pabs { pi  x t1 t2 }
  | t=papp { t }

papp:
  | t1=papp t2=pvar { app t1 t2 }
  | t=pvar { t }

pvar:
  | x=VAR { var x }
  | STAR  { sort Type }
  | SQUARE { sort Kind }
  | LPAREN t=pabs RPAREN { t }
