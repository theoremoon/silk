{
  let reserve = [
    ("def", Parser.DEF);
    ("if", Parser.IF);
    ("else", Parser.ELSE);
  ]
}
rule main = parse
  |[' ' '\n']+ { main lexbuf } (* skip space *)
  |['0'-'9']+ as num
  { Parser.NUM (int_of_string num) }

  |"="  { Parser.EQUAL }
  |"+"  { Parser.PLUS }
  |"*"  { Parser.ASTERISK }
  |"-"  { Parser.MINUS }
  |"/"  { Parser.SLASH }

  |"("  { Parser.LPAREN }
  |")"  { Parser.RPAREN }
  |"{"  { Parser.LBRACE }
  |"}"  { Parser.RBRACE }

  |"=="  { Parser.EQEQ }
  |"!="  { Parser.NOTEQ }
  |"<"  { Parser.LANGLE }
  |">"  { Parser.RANGLE }
  |"<="  { Parser.LANGLE_EQ }
  |">="  { Parser.RANGLE_EQ }

  |","  { Parser.COMMA }
  |":"  { Parser.COLON }

  |['a'-'z''A'-'z''0'-'9''_']+ as id
  {
    try List.assoc id reserve
    with _ -> Parser.ID (id)
  }

  |eof { Parser.EOF }
