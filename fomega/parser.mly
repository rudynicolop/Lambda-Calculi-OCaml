%{
    open Syntax
%}

(** Tokens. *)
%token LPAREN RPAREN LBRACK RBRACK EOF
%token STAR KARROW
%token FORALL DOUBLECOLON COMMA ARROW
%token FUN COLON DOT LAM
%token <string> VAR

(** Start symbol. *)
%start <p_term> prog

(** Type annotations. *)
%type <p_term> ptypabs
%type <p_term> pabs
%type <p_term> papp
%type <p_term> ptypapp
%type <p_term> pvar
%type <p_typ> ptabs
%type <p_typ> ptforall
%type <p_typ> ptarrow
%type <p_typ> ptapp
%type <p_typ> ptvar
%type <kind> pkarrow
%type <kind> pkstar

%%

prog:
  | e=ptypabs EOF { e }

ptypabs:
  | LAM x=VAR DOUBLECOLON k=pkarrow DOT e=ptypabs { typabs x k e }
  | e=pabs { e }

pabs:
  | FUN x=VAR COLON t=ptabs DOT e=pabs { abs x t e }
  | e=papp { e }

papp:
  | e1=papp e2=ptypapp { app e1 e2 }
  | e=ptypapp { e }

ptypapp:
  | e=ptypapp LBRACK t=ptabs RBRACK { typapp e t }
  | e=pvar { e }

pvar:
  | x=VAR { var x }
  | LPAREN e=ptypabs RPAREN { e }

ptabs:
  | FUN x=VAR DOUBLECOLON k=pkarrow DOT t=ptabs { tabs x k t }
  | t=ptforall { t }

ptforall:
  | FORALL x=VAR DOUBLECOLON k=pkarrow COMMA t=ptforall { tforall x k t }
  | t=ptarrow { t }

ptarrow:
  | t1=ptapp ARROW t2=ptarrow { tarrow t1 t2 }
  | t=ptapp { t }

ptapp:
  | t1=ptapp t2=ptvar { tapp t1 t2 }
  | t=ptvar { t }

ptvar:
  | x=VAR { tvar x }
  | LPAREN t=ptabs RPAREN { t }

pkarrow:
  | k1=pkstar KARROW k2=pkarrow { karrow k1 k2 }
  | k=pkstar { k }

pkstar:
  | STAR { KStar }
  | LPAREN k=pkarrow RPAREN { k }
