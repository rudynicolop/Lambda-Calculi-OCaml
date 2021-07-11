%{
    open Syntax
%}

(** Tokens *)
%token LPAREN RPAREN EOF
%token ARROW BOT
%token FUN COLON MAPSTO
%token <string> VAR

(** Start symbol *)
%start <p_expr> prog

(** Type Annotations *)
%type <p_expr> abs_expr
%type <p_expr> app_expr
%type <p_expr> var_expr
%type <typ> arrow_typ
%type <typ> bot_typ

%%

prog:
  | e=abs_expr EOF { e }

abs_expr:
  | FUN x=VAR COLON t=arrow_typ MAPSTO e=abs_expr { Abs(x,t,e) }
  | e=app_expr { e }

app_expr:
  | e1=app_expr e2=var_expr { App(e1,e2) }
  | e=var_expr { e }

var_expr:
  | x=VAR { Var x }
  | LPAREN e=abs_expr RPAREN { e }

arrow_typ:
  | t1=bot_typ ARROW t2=arrow_typ { Arrow(t1,t2) }
  | t=bot_typ { t }

bot_typ:
  | BOT { False }
  | LPAREN t=arrow_typ RPAREN { t }
