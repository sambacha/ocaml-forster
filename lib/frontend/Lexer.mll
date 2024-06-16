{
  open Forester_prelude
  let raise_err lexbuf =
    let loc = Asai.Range.of_lexbuf lexbuf in
    Forester_core.Reporter.fatalf ~loc Forester_core.Reporter.Message.Parse_error "unrecognized token `%s`" @@
    String.escaped @@ Lexing.lexeme lexbuf
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let cmd = (alpha) (alpha|digit|'-')*
let escape_cmd = ('\\' | ',' | '\"' | '\'' | '`' | '_' | ';' | '#' | '{' | '}' | '[' | ']' | ' ')

let xml_base_ident = (alpha) (alpha|digit|'-'|'_')*
let xml_qname = (xml_base_ident ':' xml_base_ident) | xml_base_ident
let addr = (alpha) (alpha|digit|'_'|'-')*
let wschar = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let text = [^ ' ' '%' '#' '\\' '{' '}' '[' ']' '(' ')' '<' '>' '\r' '\n' '/' '$' '#' ',' ';' ':']+

let verbatim_herald = [^ ' ' '\t' '\r' '\n' '|' ]+
let verbatim_herald_sep = '|'

rule token =
  parse
  | "%" { comment lexbuf }
  | "##{" { Grammar.HASH_HASH_LBRACE }
  | "#{" { Grammar.HASH_LBRACE }
  | "#" { Grammar.HASH }
  | "$" { Grammar.DOLLAR }
  | "," { Grammar.COMMA }
  | ";" { Grammar.SEMI }
  | ":" { Grammar.COLON }

  | "\\%" { Grammar.TEXT "%"}
  | "\\put?" { Grammar.DEFAULT }
  | "\\query/and" {Grammar.QUERY_AND }
  | "\\query/or" {Grammar.QUERY_OR }
  | "\\query/not" {Grammar.QUERY_NOT }
  | "\\query/author" {Grammar.QUERY_AUTHOR }
  | "\\query/tag" {Grammar.QUERY_TAG }
  | "\\query/taxon" {Grammar.QUERY_TAXON }
  | "\\query/meta" {Grammar.QUERY_META }
  | "\\xmlns:"
  { let prefix = xml_base_ident lexbuf in
    DECL_XMLNS prefix }

  | "\\<"
    { BSLASH_LANGLE }

  | "\\" { ident_start lexbuf}

  | "/" { Grammar.SLASH }
  | "#" { Grammar.HASH }

  | '{' { Grammar.LBRACE }
  | '}' { Grammar.RBRACE }
  | '[' { Grammar.LSQUARE }
  | ']' { Grammar.RSQUARE }
  | '(' { Grammar.LPAREN }
  | ')' { Grammar.RPAREN }
  | '<' { Grammar.LANGLE }
  | '>' { Grammar.RANGLE }

  | text { Grammar.TEXT (Lexing.lexeme lexbuf) }
  | wschar+ { Grammar.WHITESPACE (Lexing.lexeme lexbuf) }
  | newline { Lexing.new_line lexbuf; Grammar.WHITESPACE (Lexing.lexeme lexbuf) }
  | eof { Grammar.EOF }
  | _ { raise_err lexbuf }

and ident_start =
  parse
  | cmd as x
    { match x with
      | "scope" -> Grammar.SCOPE
      | "open" -> Grammar.OPEN
      | "namespace" -> Grammar.NAMESPACE
      | "subtree" -> Grammar.SUBTREE
      | "import" -> Grammar.IMPORT
      | "export" -> Grammar.EXPORT
      | "let" -> Grammar.LET
      | "def" -> Grammar.DEF
      | "alloc" -> Grammar.ALLOC
      | "object" -> Grammar.OBJECT
      | "patch" -> Grammar.PATCH
      | "call" -> Grammar.CALL
      | "put" -> Grammar.PUT
      | "get" -> Grammar.GET
      | "query" -> Grammar.QUERY_TREE
      | "verb" -> custom_verbatim_herald lexbuf
      | "startverb" -> custom_verbatim "\\stopverb" (Buffer.create 2000) lexbuf
      | _ -> BSLASH_IDENT x }

  | escape_cmd as c { Grammar.BSLASH_IDENT (String.make 1 c) }

  | _ { raise_err lexbuf }

and comment =
  parse
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | eof { Grammar.EOF }
  | _ { comment lexbuf }

and custom_verbatim_herald =
  parse
  | verbatim_herald as herald
    { let buffer = Buffer.create 2000 in
      eat_verbatim_herald_sep (custom_verbatim herald buffer) lexbuf }
  | _ as c
    { raise_err lexbuf }

and eat_verbatim_herald_sep kont =
  parse
  | verbatim_herald_sep
    { kont lexbuf }
  | _ as c
   { raise_err lexbuf }

and custom_verbatim herald buffer =
  parse
  | newline as c
    { Lexing.new_line lexbuf;
      Buffer.add_string buffer c;
      custom_verbatim herald buffer lexbuf; }
  | _ as c
    { Buffer.add_char buffer c;
      let buff_len = Buffer.length buffer in
      let herald_len = String.length herald in
      let offset = buff_len - herald_len in
      if offset >= 0 && Buffer.sub buffer offset herald_len = herald then
        let text =
          String_util.trim_trailing_whitespace @@
          String_util.trim_newlines @@
          Buffer.sub buffer 0 offset
        in
        Grammar.VERBATIM text
      else
        custom_verbatim herald buffer lexbuf }

and xml_qname =
  parse
  | xml_qname as qname { qname }
  | _ { raise_err lexbuf  }


and xml_base_ident =
  parse
  | xml_base_ident as x { x }
  | _ { raise_err lexbuf}


and rangle =
  parse
  | ">" { () }
  | _ { raise_err lexbuf }

