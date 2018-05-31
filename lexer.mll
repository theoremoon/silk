{
  let reserve = [
    ("def", Parser.DEF)
  ]
}
rule main = parse
  |[' ' '\n']+ { main lexbuf } (* skip space *)
  |['0'-'9']+  { Parser.NUM (int_of_string (Lexing.lexeme lexbuf)) }
  |"="  { Parser.EQUAL }
  |"+"  { Parser.PLUS }
  |"*"  { Parser.ASTERISK }
  |"-"  { Parser.MINUS }
  |"/"  { Parser.SLASH }
  |"("  { Parser.LPAREN }
  |")"  { Parser.RPAREN }
  |"{"  { Parser.LBRACE }
  |"}"  { Parser.RBRACE }
  |eof { Parser.EOF }
  |['a'-'z']['a'-'z''A'-'z''0'-'9''_']* {
    let id = Lexing.lexeme lexbuf in
    try List.assoc id reserve
    with _ -> Parser.ID (id)
  }
