%{
    open Ast
%}

(** Tokens *)
%token LPAREN RPAREN EOF
%token LAMBDA ARROW
%token <string> VAR

(** Start symbol *)
%start <p_expr> prog

(** Type Annotations *)
%type <p_expr> lambda_expr
%type <p_expr> app_expr
%type <p_expr> var_expr

%%

prog:
  | e=lambda_expr EOF { e }

lambda_expr:
  | LAMBDA x=VAR ARROW e=lambda_expr { Lam(x,e) }
  | e=app_expr { e }

app_expr:
  | e1=app_expr e2=var_expr { App(e1,e2) }
  | e=var_expr { e }

var_expr:
  | x=VAR { Var x }
  | LPAREN e=lambda_expr RPAREN { e }
