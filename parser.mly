%{
  open Syntax
%}

%token PLUS EOL
%token <int> NUM

%start toplevel
%type <Syntax.program> toplevel

%%

toplevel:
  |Expr EOL { Exp ($1) }

Expr:
  |Expr PLUS Factor { Add ($1, $3) }
  |Factor { $1 }

Factor:
  |NUM { Int $1 }

