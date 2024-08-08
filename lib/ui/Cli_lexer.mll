{
  open Forester_prelude
  open Lexing
  open Parser
  exception Syntax_error of string
}

let wschar = [' ' '\t']
let whitespace = wschar+

let text = [^ ' ' '%' '#' '\\' '{' '}' '[' ']' '(' ')' '\r' '\n']+

rule skip_whitespace kont =
  parse
  | whitespace { skip_whitespace kont lexbuf }

and token =
  parse
  | "edit" { Parser.EDIT  }
  | "view" { Parser.VIEW  }
  | "help" { Parser.HELP  }
  | "quit" { Parser.QUIT  }
  | "ls"   { Parser.LS    }
  | wschar { token lexbuf }
  | text   { Parser.TEXT (lexeme lexbuf)}
  | eof    { EOF }
  | _ { raise @@ Syntax_error (lexeme lexbuf)}
