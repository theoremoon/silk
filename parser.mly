%{
  open Syntax
%}

%token EQUAL
%token COMMA
%token PLUS MINUS ASTERISK SLASH
%token LANGLE RANGLE LANGLE_EQ RANGLE_EQ EQEQ NOTEQ
%token LPAREN RPAREN LBRACE RBRACE
%token EOF
%token DEF
%token IF ELSE
%token <int> NUM
%token <string> ID

%start toplevel
%type <Syntax.exp> toplevel

%%

toplevel:
  |Expr* EOF { MultiExpr($1) }

Expr:
  |AssignExpr { $1 }

AssignExpr:
  |DEF id=ID EQUAL exp=Arithmetic { Assign(id, exp) }
  |Arithmetic { $1 }

Arithmetic:
  |Arithmetic PLUS Term { Call ("+", [$1; $3]) }
  |Arithmetic MINUS Term { Call ("-", [$1; $3]) }
  |Term { $1 }

Term:
  |Term ASTERISK Factor { Call ("*", [$1; $3]) }
  |Term SLASH Factor { Call ("/", [$1; $3]) }
  |Compare { $1 }

Compare:
  |Compare EQEQ Factor { Call ("==", [$1; $3]) }
  |Compare NOTEQ Factor { Call ("!=", [$1; $3]) }
  |Compare LANGLE Factor { Call ("<", [$1; $3]) }
  |Compare RANGLE Factor { Call (">", [$1; $3]) }
  |Compare LANGLE_EQ Factor { Call ("<=", [$1; $3]) }
  |Compare RANGLE_EQ Factor { Call (">=", [$1; $3]) }
  |Factor { $1 }

Factor:
  |MINUS Factor { Call("__neg", [$2]) }
  |Num { $1 }
  |IfExpr { $1 }
  |fname = ID LPAREN args = separated_list(COMMA, Expr) RPAREN { Call (fname, args) }
  |LBRACE list(Expr) RBRACE  { MultiExpr ( $2 ) }
  |LPAREN Expr RPAREN  { $2 }
  |DefExpr { $1 }

DefExpr:
  |DEF name = ID LPAREN args = separated_list(COMMA, ID) RPAREN body = Expr { Defun(name, args, body) }

IfExpr:
  |IF cond = Expr t = Expr ELSE e = Expr { If(cond, t, e) }

Num:
  |ID { Var $1 }
  |NUM { Int $1 }

