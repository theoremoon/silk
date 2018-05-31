%{
  open Syntax
%}

%token EQUAL
%token PLUS MINUS ASTERISK SLASH
%token LPAREN RPAREN
%token EOL EOF
%token <int> NUM
%token <string> VAR

%start toplevel
%type <Syntax.statement list> toplevel

%%

toplevel:
  |Stmt* EOF { $1 }

Stmt:
  |AssignExpr EOL { Exp ($1) }

AssignExpr:
  |VAR EQUAL Expr { Assign($1, $3) }
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

Num:
  |VAR { Var $1 }
  |NUM { Int $1 }

