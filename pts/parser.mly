%{
    open Syntax
%}

(** Tokens. *)
%token LPAREN RPAREN EOF
%token STAR SQUARE TRIANGLE SUC
%token FUN PI COLON DOT
%token <string> VAR

(** Start symbol. *)
%start <p_term> prog

(** Type annotations. *)
%type <p_term> pabs
%type <p_term> papp
%type <p_term> pvar
%type <sort> psort

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
  | s=psort { sort s }
  | LPAREN t=pabs RPAREN { t }

psort:
  | STAR  { Prop }
  | SQUARE { Suc Prop }
  | TRIANGLE { Suc (Suc Prop) }
  | SUC s=psort { Suc s }
