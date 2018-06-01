%{
  open Syntax
%}

%token EQUAL
%token PLUS MINUS ASTERISK SLASH
%token LPAREN RPAREN LBRACE RBRACE
%token EOF
%token DEF
%token <int> NUM
%token <string> ID

%start toplevel
%type <Syntax.stmt list> toplevel

%%

toplevel:
  |Stmt* EOF { $1 }

Stmt:
  |AssignExpr { Exp ($1) }
  |DEF ID LBRACE Stmt* RBRACE { Defun($2, $4) }

AssignExpr:
  |ID EQUAL Expr { Assign($1, $3) }
  |Expr { $1 }

Expr:
  |Expr PLUS Term { Add ($1, $3) }
  |Expr MINUS Term { Sub ($1, $3) }
  |Term { $1 }

Term:
  |Term ASTERISK Factor { Mul ($1, $3) }
  |Term SLASH Factor { Div ($1, $3) }
  |Factor { $1 }

Factor:
  |MINUS Num { Neg $2 }
  |Num { $1 }
  |LPAREN Expr RPAREN { $2 }
  |ID LPAREN Expr RPAREN { Call ($1, $3) }

Num:
  |ID { Var $1 }
  |NUM { Int $1 }

