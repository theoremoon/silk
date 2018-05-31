rule main = parse
  |[' ']+ { main lexbuf } (* skip space *)
  |['-']?['0'-'9']+  { Parser.NUM (int_of_string (Lexing.lexeme lexbuf)) }
  |"+"  { Parser.PLUS }
  |"*"  { Parser.ASTERISK }
  |"\n" { Parser.EOL }
