rule main = parse
  |[' ']+ { main lexbuf } (* skip space *)
  |['0'-'9']+  { Parser.NUM (int_of_string (Lexing.lexeme lexbuf)) }
  |['a'-'z']['a'-'z''A'-'z''0'-'9''_']* { Parser.VAR (Lexing.lexeme lexbuf) }
  |"="  { Parser.EQUAL }
  |"+"  { Parser.PLUS }
  |"*"  { Parser.ASTERISK }
  |"-"  { Parser.MINUS }
  |"/"  { Parser.SLASH }
  |"("  { Parser.LPAREN }
  |")"  { Parser.RPAREN }
  |"\n" { Parser.EOL }
  |eof { Parser.EOF }
