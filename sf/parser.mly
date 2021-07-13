%{
    open Syntax
%}

(** Tokens *)
%token LPAREN RPAREN LBRACK RBRACK EOF
%token FORALL COMMA ARROW
%token LAM DOT FUN COLON MAPSTO
%token <string> VAR

(** Start symbol *)
%start <p_expr> prog

(** Type Annotations *)
%type <p_expr> typabs_expr
%type <p_expr> abs_expr
%type <p_expr> app_expr
%type <p_expr> typapp_expr
%type <p_expr> var_expr
%type <p_typ> tforall_typ
%type <p_typ> tarrow_typ
%type <p_typ> tvar_typ

%%

prog:
  | e=typabs_expr EOF { e }

typabs_expr:
  | LAM x=VAR DOT e=typabs_expr { typabs x e }
  | e=abs_expr { e }

abs_expr:
  | FUN x=VAR COLON t=tforall_typ MAPSTO e=abs_expr { abs x t e }
  | e=app_expr { e }

app_expr:
  | e1=app_expr e2=typapp_expr { app e1 e2 }
  | e=typapp_expr { e }

typapp_expr:
  | e=typapp_expr LBRACK t=tforall_typ RBRACK { typapp e t }
  | e=var_expr { e }

var_expr:
  | x=VAR { var x }
  | LPAREN e=typabs_expr RPAREN { e }

tforall_typ:
  | FORALL x=VAR COMMA t=tforall_typ { tforall x t }
  | t=tarrow_typ { t }

tarrow_typ:
  | t1=tvar_typ ARROW t2=tarrow_typ { tarrow t1 t2 }
  | t=tvar_typ { t }

tvar_typ:
  | x=VAR { tvar x }
  | LPAREN t=tforall_typ RPAREN { t }
