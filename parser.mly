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
%type <Syntax.stmt list> toplevel

%%

toplevel:
  |Stmt* EOF { $1 }

Stmt:
  |AssignExpr { Exp ($1) }
  |DEF name = ID LPAREN args = separated_list(COMMA, ID) RPAREN body = Expr { Defun(name, args, body) }

Expr:
  |AssignExpr { $1 }

AssignExpr:
  |ID EQUAL Arithmetic { Assign($1, $3) }
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
  |Compare EQEQ Factor { CmpOp ("==", $1, $3) }
  |Compare NOTEQ Factor { CmpOp ("!=", $1, $3) }
  |Compare LANGLE Factor { CmpOp ("<", $1, $3) }
  |Compare RANGLE Factor { CmpOp (">", $1, $3) }
  |Compare LANGLE_EQ Factor { CmpOp ("<=", $1, $3) }
  |Compare RANGLE_EQ Factor { CmpOp (">=", $1, $3) }
  |Factor { $1 }

Factor:
  |MINUS Factor { Neg $2 }
  |Num { $1 }
  |IfExpr { $1 }
  |LPAREN Expr RPAREN { $2 }
  |fname = ID LPAREN args = separated_list(COMMA, Expr) RPAREN { Call (fname, args) }
  |LBRACE Expr* RBRACE  { MultiExpr ( $2 ) }

IfExpr:
  |IF cond = Expr LBRACE t = Expr RBRACE ELSE LBRACE e = Expr RBRACE { If(cond, t, e) }

Num:
  |ID { Var $1 }
  |NUM { Int $1 }

