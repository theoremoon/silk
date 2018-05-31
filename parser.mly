%{
  open Syntax
%}

%token PLUS ASTERISK EOL
%token <int> NUM

%start toplevel
%type <Syntax.program> toplevel

%%

toplevel:
  |Expr EOL { Exp ($1) }

Expr:
  |Expr PLUS Term { Add ($1, $3) }
  |Term { $1 }

Term:
  |Term ASTERISK Factor { Mult($1, $3) }
  |Factor { $1 }

Factor:
  |NUM { Int $1 }

