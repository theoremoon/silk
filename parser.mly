%{
  open Syntax
%}
%token <int> NUM

%start toplevel
%type <Syntax.program> toplevel

%%

toplevel:
  |NUM { Int $1 }

