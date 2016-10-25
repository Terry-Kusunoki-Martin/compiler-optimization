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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Expr

# 37 "parser.ml"
let yytransl_const = [|
  259 (* MAIN *);
  260 (* DEF *);
  261 (* ADD1 *);
  262 (* SUB1 *);
  263 (* LPARENNOSPACE *);
  264 (* LPARENSPACE *);
  265 (* RPAREN *);
  266 (* LET *);
  267 (* IN *);
  268 (* EQUAL *);
  269 (* COMMA *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* TIMES *);
  273 (* IF *);
  274 (* COLON *);
  275 (* ELSECOLON *);
  276 (* TRUE *);
  277 (* FALSE *);
  278 (* ISBOOL *);
  279 (* ISNUM *);
  280 (* EQEQ *);
  281 (* LESS *);
  282 (* GREATER *);
  283 (* PRINT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\006\000\006\000\007\000\007\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\005\000\005\000\005\000\009\000\009\000\
\010\000\010\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\005\000\001\000\003\000\001\000\003\000\004\000\004\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\001\000\001\000\004\000\006\000\001\000\006\000\007\000\
\001\000\002\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\000\000\004\000\005\000\000\000\
\000\000\000\000\000\000\002\000\003\000\007\000\008\000\006\000\
\037\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\000\
\017\000\000\000\000\000\000\000\019\000\018\000\000\000\000\000\
\000\000\000\000\020\000\021\000\022\000\023\000\024\000\025\000\
\035\000\000\000\016\000\000\000\000\000\000\000\000\000\028\000\
\000\000\015\000\014\000\000\000\000\000\000\000\000\000\000\000\
\012\000\031\000\000\000\010\000\029\000\032\000"

let yydgoto = "\002\000\
\017\000\018\000\019\000\029\000\042\000\062\000\043\000\021\000\
\022\000\023\000"

let yysindex = "\004\000\
\032\255\000\000\000\000\255\254\009\255\000\000\000\000\078\255\
\078\255\013\255\078\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\010\255\018\000\244\254\015\255\078\255\055\255\
\014\255\011\255\016\255\012\255\018\255\005\255\078\255\000\000\
\124\255\124\255\124\255\124\255\124\255\124\255\000\000\026\000\
\000\000\019\255\022\255\007\255\000\000\000\000\078\255\078\255\
\078\255\026\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\078\255\000\000\028\255\025\255\035\255\037\255\000\000\
\008\255\000\000\000\000\043\255\078\255\033\255\013\255\078\255\
\000\000\000\000\078\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\028\000\101\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\049\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\057\255\000\000\000\000\056\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\254\255\255\255\002\000\015\000\056\000\
\000\000\059\000"

let yytablesize = 311
let yytable = "\020\000\
\027\000\033\000\034\000\035\000\001\000\024\000\026\000\027\000\
\060\000\030\000\025\000\036\000\037\000\038\000\028\000\061\000\
\031\000\032\000\005\000\045\000\044\000\040\000\049\000\047\000\
\046\000\057\000\072\000\030\000\048\000\050\000\059\000\058\000\
\003\000\004\000\066\000\005\000\006\000\007\000\008\000\009\000\
\068\000\010\000\069\000\070\000\060\000\063\000\064\000\065\000\
\011\000\071\000\075\000\012\000\013\000\014\000\015\000\003\000\
\004\000\013\000\016\000\006\000\007\000\008\000\009\000\041\000\
\010\000\011\000\009\000\074\000\076\000\073\000\077\000\011\000\
\067\000\078\000\012\000\013\000\014\000\015\000\003\000\004\000\
\039\000\016\000\006\000\007\000\008\000\009\000\000\000\010\000\
\051\000\052\000\053\000\054\000\055\000\056\000\011\000\000\000\
\000\000\012\000\013\000\014\000\015\000\033\000\033\000\000\000\
\016\000\033\000\033\000\033\000\033\000\000\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\033\000\000\000\000\000\
\033\000\033\000\033\000\033\000\003\000\004\000\000\000\033\000\
\006\000\007\000\008\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\013\000\014\000\015\000\000\000\000\000\000\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\027\000\027\000\000\000\027\000\027\000\027\000\000\000\
\027\000\027\000\027\000\027\000\000\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\030\000\030\000\000\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\030\000\000\000\
\030\000\000\000\000\000\000\000\030\000\030\000\030\000\030\000\
\030\000\030\000\030\000\000\000\000\000\000\000\030\000"

let yycheck = "\001\000\
\000\000\014\001\015\001\016\001\001\000\007\001\008\000\009\000\
\002\001\011\000\002\001\024\001\025\001\026\001\002\001\009\001\
\007\001\000\000\004\001\009\001\007\001\023\000\018\001\012\001\
\009\001\000\000\019\001\000\000\011\001\031\000\009\001\013\001\
\001\001\002\001\009\001\004\001\005\001\006\001\007\001\008\001\
\013\001\010\001\018\001\009\001\002\001\047\000\048\000\049\000\
\017\001\013\001\018\001\020\001\021\001\022\001\023\001\001\001\
\002\001\009\001\027\001\005\001\006\001\007\001\008\001\009\001\
\010\001\009\001\011\001\069\000\071\000\068\000\072\000\017\001\
\058\000\075\000\020\001\021\001\022\001\023\001\001\001\002\001\
\022\000\027\001\005\001\006\001\007\001\008\001\255\255\010\001\
\033\000\034\000\035\000\036\000\037\000\038\000\017\001\255\255\
\255\255\020\001\021\001\022\001\023\001\001\001\002\001\255\255\
\027\001\005\001\006\001\007\001\008\001\255\255\010\001\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\255\255\255\255\
\020\001\021\001\022\001\023\001\001\001\002\001\255\255\027\001\
\005\001\006\001\007\001\008\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\027\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\005\001\006\001\255\255\
\008\001\009\001\010\001\011\001\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\001\001\002\001\255\255\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\013\001\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\255\255\255\255\027\001"

let yynames_const = "\
  MAIN\000\
  DEF\000\
  ADD1\000\
  SUB1\000\
  LPARENNOSPACE\000\
  LPARENSPACE\000\
  RPAREN\000\
  LET\000\
  IN\000\
  EQUAL\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  IF\000\
  COLON\000\
  ELSECOLON\000\
  TRUE\000\
  FALSE\000\
  ISBOOL\000\
  ISNUM\000\
  EQEQ\000\
  LESS\000\
  GREATER\000\
  PRINT\000\
  EOF\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 21 "parser.mly"
        ( ENumber(_1) )
# 254 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "parser.mly"
         ( EBool(true) )
# 260 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "parser.mly"
          ( EBool(false) )
# 266 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    Obj.repr(
# 26 "parser.mly"
         ( Add1 )
# 272 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
         ( Sub1 )
# 278 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
          ( Print )
# 284 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
           ( IsBool )
# 290 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "parser.mly"
          ( IsNum )
# 296 "parser.ml"
               : 'prim1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                  ( [(_1, _3)] )
# 304 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'binds) in
    Obj.repr(
# 34 "parser.mly"
                              ( (_1, _3)::_5 )
# 313 "parser.ml"
               : 'binds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 37 "parser.mly"
       ( [_1] )
# 320 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'ids) in
    Obj.repr(
# 38 "parser.mly"
                 ( _1::_3 )
# 328 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
         ( [_1] )
# 335 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprs) in
    Obj.repr(
# 42 "parser.mly"
                     ( _1::_3 )
# 343 "parser.ml"
               : 'exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'prim1) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 45 "parser.mly"
                                    ( EPrim1(_1, _3) )
# 351 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'exprs) in
    Obj.repr(
# 46 "parser.mly"
                                  ( EApp(_1, _3) )
# 359 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 47 "parser.mly"
                            ( EApp(_1, []) )
# 366 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                            ( _2 )
# 373 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                              ( _2 )
# 380 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 50 "parser.mly"
                               ( EPrim2(Plus, _1, _3) )
# 388 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 51 "parser.mly"
                                ( EPrim2(Minus, _1, _3) )
# 396 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 52 "parser.mly"
                                ( EPrim2(Times, _1, _3) )
# 404 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 53 "parser.mly"
                               ( EPrim2(Equal, _1, _3) )
# 412 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 54 "parser.mly"
                               ( EPrim2(Less, _1, _3) )
# 420 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'binop_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 55 "parser.mly"
                                  ( EPrim2(Greater, _1, _3) )
# 428 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 56 "parser.mly"
          ( _1 )
# 435 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 57 "parser.mly"
       ( EId(_1) )
# 442 "parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'binds) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                      ( ELet(_2, _4) )
# 450 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                      ( EIf(_2, _4, _6) )
# 459 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 62 "parser.mly"
               ( _1 )
# 466 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                                           ( DFun(_2, [], _6) )
# 474 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ids) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                                               ( DFun(_2, _4, _7) )
# 483 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 69 "parser.mly"
         ( [_1] )
# 490 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 70 "parser.mly"
               ( _1::_2 )
# 498 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                   ( Program(_1, _2) )
# 506 "parser.ml"
               : Expr.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 74 "parser.mly"
             ( Program([], _1) )
# 513 "parser.ml"
               : Expr.program))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr.program)
;;
# 77 "parser.mly"

# 540 "parser.ml"
