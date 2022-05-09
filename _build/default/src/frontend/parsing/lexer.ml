# 1 "src/frontend/parsing/lexer.mll"
 
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let position lexbuf.lex_curr_pos in
    lexbuf.lex_curr_pos <-
      { position with pos_bol = lexbuf.lex_curr_pos;
          pos_lnum = position.pos_lnum + 1
      }

# 16 "src/frontend/parsing/lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\202\255\203\255\204\255\205\255\206\255\001\000\000\000\
    \000\000\003\000\002\000\005\000\013\000\002\000\000\000\002\000\
    \001\000\023\000\019\000\011\000\001\000\230\255\231\255\232\255\
    \233\255\234\255\235\255\236\255\237\255\001\000\004\000\003\000\
    \004\000\005\000\248\255\249\255\006\000\251\255\252\255\006\000\
    \010\000\254\255\255\255\242\255\247\255\228\255\244\255\243\255\
    \241\255\239\255\238\255\229\255\018\000\024\000\227\255\217\255\
    \000\000\000\000\226\255\225\255\019\000\027\000\001\000\224\255\
    \021\000\036\000\219\255\218\255\000\000\023\000\024\000\223\255\
    \222\255\025\000\040\000\040\000\221\255\220\255\035\000\216\255\
    \046\000\041\000\031\000\030\000\052\000\034\000\050\000\215\255\
    \052\000\055\000\039\000\055\000\057\000\214\255\043\000\051\000\
    \055\000\062\000\213\255\212\255\055\000\062\000\211\255\049\000\
    \049\000\053\000\058\000\210\255\052\000\069\000\209\255\066\000\
    \072\000\208\255\068\000\066\000\074\000\207\255\002\000\253\255\
    \254\255\255\255\004\000\059\000\251\255\252\255\253\255\006\000\
    \001\000\052\000\255\255\254\255\143\000\244\255\144\000\145\000\
    \255\255\247\255\248\255\249\255\250\255\251\255\252\255\253\255\
    \254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\255\255\255\255\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\053\000\053\000\053\000\
    \053\000\053\000\053\000\053\000\053\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\053\000\053\000\015\000\
    \010\000\009\000\255\255\255\255\005\000\255\255\255\255\002\000\
    \053\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\002\000\255\255\255\255\255\255\255\255\004\000\
    \004\000\004\000\255\255\255\255\255\255\255\255\009\000\010\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\000\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\000\000\000\000\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\000\000\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\255\255\255\255\255\255\000\000\000\000\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\000\000\255\255\
    \255\255\000\000\255\255\255\255\255\255\000\000\119\000\000\000\
    \000\000\000\000\255\255\124\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\000\000\000\000\134\000\000\000\134\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\005\000\005\000\121\000\006\000\121\000\122\000\
    \126\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\031\000\003\000\004\000\131\000\034\000\029\000\050\000\
    \026\000\025\000\036\000\038\000\028\000\037\000\027\000\035\000\
    \044\000\000\000\058\000\000\000\059\000\063\000\000\000\000\000\
    \000\000\000\000\040\000\000\000\033\000\039\000\032\000\051\000\
    \048\000\047\000\046\000\043\000\042\000\126\000\057\000\041\000\
    \127\000\056\000\000\000\000\000\000\000\062\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\024\000\000\000\023\000\129\000\000\000\
    \000\000\069\000\019\000\016\000\015\000\014\000\017\000\103\000\
    \114\000\018\000\100\000\078\000\073\000\068\000\013\000\111\000\
    \012\000\072\000\010\000\011\000\009\000\108\000\008\000\007\000\
    \061\000\055\000\052\000\022\000\030\000\021\000\020\000\081\000\
    \049\000\053\000\080\000\045\000\054\000\067\000\060\000\064\000\
    \065\000\066\000\070\000\071\000\074\000\076\000\077\000\079\000\
    \095\000\075\000\083\000\088\000\084\000\085\000\086\000\087\000\
    \082\000\089\000\090\000\091\000\092\000\093\000\099\000\096\000\
    \097\000\098\000\094\000\101\000\102\000\104\000\105\000\106\000\
    \107\000\109\000\110\000\112\000\113\000\115\000\116\000\117\000\
    \130\000\136\000\255\255\137\000\000\000\000\000\000\000\128\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \144\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\135\000\255\255\143\000\000\000\000\000\
    \000\000\000\000\000\000\142\000\000\000\000\000\000\000\141\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\140\000\
    \002\000\000\000\120\000\139\000\000\000\138\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\125\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\133\000\
    \255\255\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\006\000\118\000\000\000\122\000\118\000\
    \127\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\128\000\000\000\000\000\029\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \036\000\255\255\057\000\255\255\056\000\062\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\000\000\000\000\000\000\020\000\
    \031\000\032\000\033\000\039\000\040\000\123\000\018\000\040\000\
    \123\000\018\000\255\255\255\255\255\255\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\123\000\255\255\
    \255\255\068\000\000\000\000\000\000\000\000\000\000\000\010\000\
    \007\000\000\000\011\000\013\000\014\000\016\000\000\000\008\000\
    \000\000\015\000\000\000\000\000\000\000\009\000\000\000\000\000\
    \017\000\018\000\019\000\000\000\000\000\000\000\000\000\012\000\
    \030\000\052\000\012\000\033\000\053\000\060\000\017\000\061\000\
    \064\000\065\000\069\000\070\000\073\000\074\000\075\000\078\000\
    \080\000\074\000\081\000\082\000\083\000\084\000\085\000\086\000\
    \081\000\088\000\089\000\090\000\091\000\092\000\094\000\095\000\
    \096\000\097\000\080\000\100\000\101\000\103\000\104\000\105\000\
    \106\000\108\000\109\000\111\000\112\000\114\000\115\000\116\000\
    \129\000\132\000\134\000\135\000\255\255\255\255\255\255\123\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \135\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\132\000\134\000\135\000\255\255\255\255\
    \255\255\255\255\255\255\135\000\255\255\255\255\255\255\135\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\135\000\
    \000\000\255\255\118\000\135\000\255\255\135\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\123\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\132\000\
    \134\000\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read_token lexbuf =
   __ocaml_lex_read_token_rec lexbuf 0
and __ocaml_lex_read_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 33 "src/frontend/parsing/lexer.mll"
            ( DBLCOLON )
# 203 "src/frontend/parsing/lexer.ml"

  | 1 ->
# 34 "src/frontend/parsing/lexer.mll"
            ( COLONEQ )
# 208 "src/frontend/parsing/lexer.ml"

  | 2 ->
# 36 "src/frontend/parsing/lexer.mll"
            ( EQ )
# 213 "src/frontend/parsing/lexer.ml"

  | 3 ->
# 38 "src/frontend/parsing/lexer.mll"
            ( ADD )
# 218 "src/frontend/parsing/lexer.ml"

  | 4 ->
# 39 "src/frontend/parsing/lexer.mll"
            ( SUB )
# 223 "src/frontend/parsing/lexer.ml"

  | 5 ->
# 41 "src/frontend/parsing/lexer.mll"
            ( MUL )
# 228 "src/frontend/parsing/lexer.ml"

  | 6 ->
# 42 "src/frontend/parsing/lexer.mll"
            ( DIV )
# 233 "src/frontend/parsing/lexer.ml"

  | 7 ->
# 43 "src/frontend/parsing/lexer.mll"
            ( MOD )
# 238 "src/frontend/parsing/lexer.ml"

  | 8 ->
# 45 "src/frontend/parsing/lexer.mll"
            ( EXP )
# 243 "src/frontend/parsing/lexer.ml"

  | 9 ->
# 47 "src/frontend/parsing/lexer.mll"
            ( LT )
# 248 "src/frontend/parsing/lexer.ml"

  | 10 ->
# 48 "src/frontend/parsing/lexer.mll"
            ( GT )
# 253 "src/frontend/parsing/lexer.ml"

  | 11 ->
# 49 "src/frontend/parsing/lexer.mll"
            ( LTEQ )
# 258 "src/frontend/parsing/lexer.ml"

  | 12 ->
# 50 "src/frontend/parsing/lexer.mll"
            ( GTEQ )
# 263 "src/frontend/parsing/lexer.ml"

  | 13 ->
# 52 "src/frontend/parsing/lexer.mll"
            ( ISEQ )
# 268 "src/frontend/parsing/lexer.ml"

  | 14 ->
# 53 "src/frontend/parsing/lexer.mll"
            ( NOTEQ )
# 273 "src/frontend/parsing/lexer.ml"

  | 15 ->
# 55 "src/frontend/parsing/lexer.mll"
            ( NOT )
# 278 "src/frontend/parsing/lexer.ml"

  | 16 ->
# 57 "src/frontend/parsing/lexer.mll"
            ( LOR )
# 283 "src/frontend/parsing/lexer.ml"

  | 17 ->
# 59 "src/frontend/parsing/lexer.mll"
            ( LAND )
# 288 "src/frontend/parsing/lexer.ml"

  | 18 ->
# 61 "src/frontend/parsing/lexer.mll"
            ( COMMA )
# 293 "src/frontend/parsing/lexer.ml"

  | 19 ->
# 62 "src/frontend/parsing/lexer.mll"
            ( DOT )
# 298 "src/frontend/parsing/lexer.ml"

  | 20 ->
# 64 "src/frontend/parsing/lexer.mll"
            ( LPAREN )
# 303 "src/frontend/parsing/lexer.ml"

  | 21 ->
# 65 "src/frontend/parsing/lexer.mll"
            ( RPAREN )
# 308 "src/frontend/parsing/lexer.ml"

  | 22 ->
# 66 "src/frontend/parsing/lexer.mll"
            ( LBRACK )
# 313 "src/frontend/parsing/lexer.ml"

  | 23 ->
# 67 "src/frontend/parsing/lexer.mll"
            ( RBRACK )
# 318 "src/frontend/parsing/lexer.ml"

  | 24 ->
# 69 "src/frontend/parsing/lexer.mll"
            ( LBRACE )
# 323 "src/frontend/parsing/lexer.ml"

  | 25 ->
# 70 "src/frontend/parsing/lexer.mll"
            ( RBRACE )
# 328 "src/frontend/parsing/lexer.ml"

  | 26 ->
# 72 "src/frontend/parsing/lexer.mll"
            ( TILDEGT )
# 333 "src/frontend/parsing/lexer.ml"

  | 27 ->
# 73 "src/frontend/parsing/lexer.mll"
            ( LTTILDE )
# 338 "src/frontend/parsing/lexer.ml"

  | 28 ->
# 76 "src/frontend/parsing/lexer.mll"
            ( DT_BOOL )
# 343 "src/frontend/parsing/lexer.ml"

  | 29 ->
# 78 "src/frontend/parsing/lexer.mll"
            ( DT_I32 )
# 348 "src/frontend/parsing/lexer.ml"

  | 30 ->
# 79 "src/frontend/parsing/lexer.mll"
            ( DT_I64 )
# 353 "src/frontend/parsing/lexer.ml"

  | 31 ->
# 81 "src/frontend/parsing/lexer.mll"
            ( DT_F64 )
# 358 "src/frontend/parsing/lexer.ml"

  | 32 ->
# 84 "src/frontend/parsing/lexer.mll"
            ( KW_CLASS )
# 363 "src/frontend/parsing/lexer.ml"

  | 33 ->
# 85 "src/frontend/parsing/lexer.mll"
            ( KW_DO)
# 368 "src/frontend/parsing/lexer.ml"

  | 34 ->
# 86 "src/frontend/parsing/lexer.mll"
            ( KW_ELSE )
# 373 "src/frontend/parsing/lexer.ml"

  | 35 ->
# 87 "src/frontend/parsing/lexer.mll"
            ( KW_ELSIF )
# 378 "src/frontend/parsing/lexer.ml"

  | 36 ->
# 88 "src/frontend/parsing/lexer.mll"
            ( KW_FALSE )
# 383 "src/frontend/parsing/lexer.ml"

  | 37 ->
# 89 "src/frontend/parsing/lexer.mll"
            ( KW_FOR )
# 388 "src/frontend/parsing/lexer.ml"

  | 38 ->
# 90 "src/frontend/parsing/lexer.mll"
            ( KW_IF )
# 393 "src/frontend/parsing/lexer.ml"

  | 39 ->
# 91 "src/frontend/parsing/lexer.mll"
            ( KW_NIL )
# 398 "src/frontend/parsing/lexer.ml"

  | 40 ->
# 92 "src/frontend/parsing/lexer.mll"
              ( KW_PRIVATE )
# 403 "src/frontend/parsing/lexer.ml"

  | 41 ->
# 93 "src/frontend/parsing/lexer.mll"
                ( KW_PROTECTED )
# 408 "src/frontend/parsing/lexer.ml"

  | 42 ->
# 94 "src/frontend/parsing/lexer.mll"
             ( KW_PUBLIC )
# 413 "src/frontend/parsing/lexer.ml"

  | 43 ->
# 95 "src/frontend/parsing/lexer.mll"
            ( KW_PUTS )
# 418 "src/frontend/parsing/lexer.ml"

  | 44 ->
# 96 "src/frontend/parsing/lexer.mll"
            ( KW_SELF )
# 423 "src/frontend/parsing/lexer.ml"

  | 45 ->
# 97 "src/frontend/parsing/lexer.mll"
             ( KW_RETURN )
# 428 "src/frontend/parsing/lexer.ml"

  | 46 ->
# 98 "src/frontend/parsing/lexer.mll"
            ( KW_TRUE )
# 433 "src/frontend/parsing/lexer.ml"

  | 47 ->
# 99 "src/frontend/parsing/lexer.mll"
            ( KW_VOID )
# 438 "src/frontend/parsing/lexer.ml"

  | 48 ->
# 100 "src/frontend/parsing/lexer.mll"
            ( KW_WHILE )
# 443 "src/frontend/parsing/lexer.ml"

  | 49 ->
# 102 "src/frontend/parsing/lexer.mll"
         ( next_line lexbuf; read_token lexbuf )
# 448 "src/frontend/parsing/lexer.ml"

  | 50 ->
# 104 "src/frontend/parsing/lexer.mll"
            ( read_single_line_comment lexbuf )
# 453 "src/frontend/parsing/lexer.ml"

  | 51 ->
# 106 "src/frontend/parsing/lexer.mll"
            ( read_dbl_quote_string (Buffer.create 17) lexbuf )
# 458 "src/frontend/parsing/lexer.ml"

  | 52 ->
# 108 "src/frontend/parsing/lexer.mll"
        ( EOF )
# 463 "src/frontend/parsing/lexer.ml"

  | 53 ->
# 109 "src/frontend/parsing/lexer.mll"
      (raise (Lexical_error ("Lexer - Illegal Character: " ^ Lexing.lexeme lexbuf)) )
# 468 "src/frontend/parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_token_rec lexbuf __ocaml_lex_state

and read_single_line_comment lexbuf =
   __ocaml_lex_read_single_line_comment_rec lexbuf 118
and __ocaml_lex_read_single_line_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 112 "src/frontend/parsing/lexer.mll"
           ( next_line lexbuf; read_token lexbuf )
# 480 "src/frontend/parsing/lexer.ml"

  | 1 ->
# 113 "src/frontend/parsing/lexer.mll"
          ( EOF )
# 485 "src/frontend/parsing/lexer.ml"

  | 2 ->
# 114 "src/frontend/parsing/lexer.mll"
        ( read_single_line_comment lexbuf )
# 490 "src/frontend/parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_single_line_comment_rec lexbuf __ocaml_lex_state

and read_multi_line_comment lexbuf =
   __ocaml_lex_read_multi_line_comment_rec lexbuf 123
and __ocaml_lex_read_multi_line_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 117 "src/frontend/parsing/lexer.mll"
           ( incr comment_depth; read_multi_line_comment lexbuf )
# 502 "src/frontend/parsing/lexer.ml"

  | 1 ->
# 118 "src/frontend/parsing/lexer.mll"
           ( decr comment_depth; 
             if !comment_depth = 0 then () else read_multi_line_comment lexbuf )
# 508 "src/frontend/parsing/lexer.ml"

  | 2 ->
# 120 "src/frontend/parsing/lexer.mll"
           ( next_line lexbuf; read_multi_line_comment lexbuf )
# 513 "src/frontend/parsing/lexer.ml"

  | 3 ->
# 121 "src/frontend/parsing/lexer.mll"
          ( raise (Lexical_error ("Lexer - Unexpectected EOF: Block comment unterminated at EOF.")))
# 518 "src/frontend/parsing/lexer.ml"

  | 4 ->
# 122 "src/frontend/parsing/lexer.mll"
        ( read_multi_line_comment lexbuf )
# 523 "src/frontend/parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_multi_line_comment_rec lexbuf __ocaml_lex_state

and read_dbl_quote_string lexbuf =
   __ocaml_lex_read_dbl_quote_string_rec lexbuf 132
and __ocaml_lex_read_dbl_quote_string_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 125 "src/frontend/parsing/lexer.mll"
                    ( STRING (Buffer.contents buffer))
# 535 "src/frontend/parsing/lexer.ml"

  | 1 ->
# 126 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '/';     read_dbl_quote_string buffer lexbuf )
# 540 "src/frontend/parsing/lexer.ml"

  | 2 ->
# 127 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '\\';    read_dbl_quote_string buffer lexbuf )
# 545 "src/frontend/parsing/lexer.ml"

  | 3 ->
# 128 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '\b';    read_dbl_quote_string buffer lexbuf )
# 550 "src/frontend/parsing/lexer.ml"

  | 4 ->
# 129 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '\012';  read_dbl_quote_string buffer lexbuf )
# 555 "src/frontend/parsing/lexer.ml"

  | 5 ->
# 130 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '\n';    read_dbl_quote_string buffer lexbuf )
# 560 "src/frontend/parsing/lexer.ml"

  | 6 ->
# 131 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '\r';    read_dbl_quote_string buffer lexbuf )
# 565 "src/frontend/parsing/lexer.ml"

  | 7 ->
# 132 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '\t';    read_dbl_quote_string buffer lexbuf )
# 570 "src/frontend/parsing/lexer.ml"

  | 8 ->
# 133 "src/frontend/parsing/lexer.mll"
                    ( Buffer.add_char buffer '"';     read_dbl_quote_string buffer lexbuf )
# 575 "src/frontend/parsing/lexer.ml"

  | 9 ->
# 135 "src/frontend/parsing/lexer.mll"
      ( Buffer.add_string buffer (Lexing.lexeme lexbuf);
        read_string buffer lexbuf
      )
# 582 "src/frontend/parsing/lexer.ml"

  | 10 ->
# 138 "src/frontend/parsing/lexer.mll"
        ( raise (Lexical_error ("Lexer - Illegal String Character: " ^ Lexing.lexeme lexbuf)) )
# 587 "src/frontend/parsing/lexer.ml"

  | 11 ->
# 139 "src/frontend/parsing/lexer.mll"
          ( raise (Lexical_error ("Lexer - Unexpectected EOF: String unterminated at EOF.")))
# 592 "src/frontend/parsing/lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_dbl_quote_string_rec lexbuf __ocaml_lex_state

;;

