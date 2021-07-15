%{
    open Syntax
%}

(** Tokens. *)
%token LPAREN RPAREN EOF
%token STAR KARROW
%token BOT TFUN DOUBLECOLON DOT TARROW
%token FUN COLON MAPSTO
%token <string> VAR

(** Start symbol. *)
%start <p_term> prog

(** Type annotations. *)
%type <p_term> pabs
%type <p_term> papp
%type <p_term> pvar
%type <p_typ> ptarrow
%type <p_typ> ptabs
%type <p_typ> ptapp
%type <p_typ> ptbot
%type <p_typ> ptvar
%type <kind> pkarrow
%type <kind> pkstar

%%

prog:
  | e=pabs EOF { e } ;

pabs:
  | FUN x=VAR COLON t=ptarrow MAPSTO e=pabs { abs x t e }
  | e=papp { e } ;

papp:
  | e1=papp e2=pvar { app e1 e2 }
  | e=pvar { e } ;

pvar:
  | x=VAR { var x }
  | LPAREN e=pabs RPAREN { e } ;

ptarrow:
  | t1=ptabs TARROW t2=ptarrow { tarrow t1 t2 }
  | t=ptabs { t } ;

ptabs:
  | TFUN x=VAR DOUBLECOLON k=pkarrow DOT t=ptabs { tabs x k t }
  | t=ptapp { t } ;

ptapp:
  | t1=ptapp t2=ptbot { tapp t1 t2 }
  | t=ptbot { t } ;

ptbot:
  | BOT { TBot }
  | t=ptvar { t } ;

ptvar:
  | x=VAR { tvar x }
  | LPAREN t=ptarrow RPAREN { t } ;

pkarrow:
  | k1=pkstar KARROW k2=pkarrow { karrow k1 k2 }
  | k=pkstar { k } ;

pkstar:
  | STAR { KStar }
  | LPAREN k=pkarrow RPAREN { k } ;
