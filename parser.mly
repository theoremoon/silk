%{
  open Syntax
%}

%token PLUS MINUS ASTERISK SLASH
%token EOL
%token <int> NUM

%start toplevel
%type <Syntax.program> toplevel

%%

toplevel:
  |Expr EOL { Exp ($1) }

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

Num:
  |NUM { Int $1 }

