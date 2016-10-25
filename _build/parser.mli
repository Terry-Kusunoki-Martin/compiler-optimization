type token =
  | NUM of (int)
  | ID of (string)
  | MAIN
  | DEF
  | ADD1
  | SUB1
  | LPARENNOSPACE
  | LPARENSPACE
  | RPAREN
  | LET
  | IN
  | EQUAL
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | IF
  | COLON
  | ELSECOLON
  | TRUE
  | FALSE
  | ISBOOL
  | ISNUM
  | EQEQ
  | LESS
  | GREATER
  | PRINT
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr.program
