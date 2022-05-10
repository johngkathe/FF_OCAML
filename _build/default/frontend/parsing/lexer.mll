{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let position lexbuf.lex_curr_pos in
    lexbuf.lex_curr_pos <-
      { position with pos_bol = lexbuf.lex_curr_pos;
          pos_lnum = position.pos_lnum + 1
      }
}

(* Helper Regexps *)
(* Helper Whitespace *)
let crlf = '\r'? '\n' | '\n'
let tab = ['\t']+
let formfeed = '\\''u''0''0''C'
let ws = ([' ' '\t' '\r' '\n'] | formfeed)+

(* Helper Digits *)
let dec_INT = ['-']?       ['1'-'9'] ['0'-'9']* | ['0']

let dec_FLOAT = ['-']?                 ['0'-'9']* ['.'] ['0'-'9']+ | dec_INT

(* Helper ID *)
let identifier = ['a'-'z']['a'-'z''A'-'Z''_']*

let operator = identifier['!' '?']*

rule read_token = parse
  | "::"    { DBLCOLON }
  | ":="    { COLONEQ }

  | "="     { EQ }

  | "+"     { ADD }
  | "-"     { SUB }

  | "*"     { MUL }
  | "/"     { DIV }
  | "%"     { MOD }

  | "**"    { EXP }

  | "<"     { LT }
  | ">"     { GT }
  | "<="    { LTEQ }
  | ">="    { GTEQ }

  | "=="    { ISEQ }
  | "!="    { NOTEQ }

  | "!"     { NOT }

  | "||"    { LOR }

  | "&&"    { LAND }

  | ","     { COMMA }
  | "."     { DOT }

  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "["     { LBRACK }
  | "]"     { RBRACK }

  | "{"     { LBRACE }
  | "}"     { RBRACE }

  | "~>"    { TILDEGT }
  | "<~"    { LTTILDE }

  (* Data Types *)
  | "bool"  { DT_BOOL }

  | "i32"   { DT_I32 }
  | "i64"   { DT_I64 }

  | "f64"   { DT_F64 }

  (* Keywords *)
  | "class" { KW_CLASS }
  | "do"    { KW_DO}
  | "else"  { KW_ELSE }
  | "elsif" { KW_ELSIF }
  | "false" { KW_FALSE }
  | "for"   { KW_FOR }
  | "if"    { KW_IF }
  | "nil"   { KW_NIL }
  | "private" { KW_PRIVATE }
  | "protected" { KW_PROTECTED }
  | "public" { KW_PUBLIC }
  | "puts"  { KW_PUTS }
  | "self"  { KW_SELF }
  | "return" { KW_RETURN }
  | "true"  { KW_TRUE }
  | "void"  { KW_VOID }
  | "while" { KW_WHILE }

  | crlf { next_line lexbuf; read_token lexbuf }
  
  | "#"     { read_single_line_comment lexbuf }

  | '"'     { read_dbl_quote_string (Buffer.create 17) lexbuf }

  | eof { EOF }
  | _ {raise (Lexical_error ("Lexer - Illegal Character: " ^ Lexing.lexeme lexbuf)) }

  and read_single_line_comment = parse
    | crlf { next_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ { read_single_line_comment lexbuf }

  and read_multi_line_comment = parse
      "#|" { incr comment_depth; read_multi_line_comment lexbuf }
    | "|#" { decr comment_depth; 
             if !comment_depth = 0 then () else read_multi_line_comment lexbuf }
    | crlf { next_line lexbuf; read_multi_line_comment lexbuf }
    | eof { raise (Lexical_error ("Lexer - Unexpectected EOF: Block comment unterminated at EOF."))}
    | _ { read_multi_line_comment lexbuf }

and read_dbl_quote_string = parse
    | '"'           { STRING (Buffer.contents buffer)}
    | '\\' '/'      { Buffer.add_char buffer '/';     read_dbl_quote_string buffer lexbuf }
    | '\\' '\\'     { Buffer.add_char buffer '\\';    read_dbl_quote_string buffer lexbuf }
    | '\\' 'b'      { Buffer.add_char buffer '\b';    read_dbl_quote_string buffer lexbuf }
    | '\\' 'f'      { Buffer.add_char buffer '\012';  read_dbl_quote_string buffer lexbuf }
    | '\\' 'n'      { Buffer.add_char buffer '\n';    read_dbl_quote_string buffer lexbuf }
    | '\\' 'r'      { Buffer.add_char buffer '\r';    read_dbl_quote_string buffer lexbuf }
    | '\\' 't'      { Buffer.add_char buffer '\t';    read_dbl_quote_string buffer lexbuf }
    | '\\' '"'      { Buffer.add_char buffer '"';     read_dbl_quote_string buffer lexbuf }
    | [^'"' '\\']+
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        read_string buffer lexbuf
      }
    | _ { raise (Lexical_error ("Lexer - Illegal String Character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (Lexical_error ("Lexer - Unexpectected EOF: String unterminated at EOF."))}
