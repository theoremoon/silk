rule main = parse
  |['-']?['0'-'9']+  { Parser.NUM (int_of_string (Lexing.lexeme lexbuf)) }
  |"+"  { Parser.PLUS }
  |"\n" { Parser.EOL }
