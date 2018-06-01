%{
  open Syntax
%}

%token EQUAL
%token COMMA
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
  |DEF name = ID LPAREN args = separated_list(COMMA, ID) RPAREN LBRACE body = Stmt* RBRACE { Defun(name, args, body) }


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
  |fname = ID LPAREN args = separated_list(COMMA, Expr) RPAREN { Call (fname, args) }

Num:
  |ID { Var $1 }
  |NUM { Int $1 }

