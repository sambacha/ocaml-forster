%{
  open Forester_prelude
  open Forester_core

  let split_xml_qname str =
    match String.split_on_char ':' str with
    | [prefix; uname] -> Some prefix, uname
    | [uname] -> None, uname
    | _ -> failwith "split_xml_qname"
%}

%token <string> XML_ELT_IDENT
%token <string> DECL_XMLNS
%token <string> TEXT VERBATIM
%token <string> WHITESPACE
%token <string> IDENT
%token IMPORT EXPORT DEF NAMESPACE LET FUN OPEN
%token OBJECT PATCH CALL
%token SUBTREE SCOPE PUT GET DEFAULT ALLOC
%token LBRACE RBRACE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token EOF

%start <Forester_core.Code.t> main

%%

let locate(p) ==
| x = p; { Asai.Range.locate_lex $loc x }

let braces(p) == delimited(LBRACE, p, RBRACE)
let squares(p) == delimited(LSQUARE, p, RSQUARE)
let parens(p) == delimited(LPAREN, p, RPAREN)

let bvar :=
| x = TEXT; { [x] }

let bvar_with_strictness :=
| x = TEXT; {
  match String_util.explode x with
  | '~' :: chars -> Lazy, [String_util.implode chars]
  | _ -> Strict, [x]
 }

let binder == list(squares(bvar_with_strictness))

let ws_or(p) :=
| WHITESPACE; { [] }
| x = p; { [x] }

let ws_list(p) := flatten(list(ws_or(p)))

let textual_node :=
| ~ = TEXT; <Code.Text>
| ~ = WHITESPACE; <Code.Text>
| ~ = head_node; <Fun.id>

let code_expr == ws_list(locate(head_node))
let textual_expr == list(locate(textual_node))

let head_node :=
| DEF; (~,~,~) = fun_spec; <Code.Def>
| ALLOC; ~ = ident; <Code.Alloc>
| IMPORT; ~ = txt_arg; <Code.import_private>
| EXPORT; ~ = txt_arg; <Code.import_public>
| NAMESPACE; ~ = ident; ~ = braces(code_expr); <Code.Namespace>
| SUBTREE; addr = option(squares(wstext)); body = braces(ws_list(locate(head_node))); <Code.Subtree>
| FUN; ~ = binder; ~ = arg; <Code.Fun>
| LET; (~,~,~) = fun_spec; <Code.Let>
| (~,~) = ident_with_method_calls; <Code.Ident>
| SCOPE; ~ = arg; <Code.Scope>
| PUT; ~ = ident; ~ = arg; <Code.Put>
| DEFAULT; ~ = ident; ~ = arg; <Code.Default>
| GET; ~ = ident; <Code.Get>
| OPEN; ~ = ident; <Code.Open>
| name = XML_ELT_IDENT; attrs = list(xml_attr); body = arg; {
  let name = split_xml_qname name in
  Code.Xml_tag (name, attrs, body)
}
| ~ = DECL_XMLNS; ~ = txt_arg; <Code.Decl_xmlns>
| OBJECT; self = option(squares(bvar)); methods = braces(ws_list(method_decl)); { Code.Object {self;  methods } }
| PATCH; obj = braces(code_expr); self = option(squares(bvar)); methods = braces(ws_list(method_decl)); { Code.Patch {obj; self; methods} }
| CALL; ~ = braces(code_expr); ~ = txt_arg; <Code.Call>
| ~ = delimited(HASH_LBRACE, textual_expr, RBRACE); <Code.inline_math>
| ~ = delimited(HASH_HASH_LBRACE, textual_expr, RBRACE); <Code.display_math>
| ~ = braces(textual_expr); <Code.braces>
| ~ = squares(textual_expr); <Code.squares>
| ~ = parens(textual_expr); <Code.parens>
| ~ = VERBATIM; <Code.Verbatim>


let method_decl :=
| k = squares(TEXT); list(WHITESPACE); v = arg; { k, v }

let xml_attr :=
| k = squares(TEXT); v = arg; { (split_xml_qname k, v) }

let ident :=
| ident = IDENT;
 { String.split_on_char '/' ident }

let ident_with_method_calls :=
| ident = IDENT;
  { match String.split_on_char '#' ident with
    | [x] -> String.split_on_char '/' x, []
    | "" :: xs -> ["#"], List.filter (fun x -> x <> "") xs
    | x :: xs -> String.split_on_char '/' x, List.filter (fun x -> x <> "") xs
    | [] -> [], []
   }


let ws_or_text :=
| x = TEXT; { x }
| x = WHITESPACE; { x }

let wstext :=
| xs = list(ws_or_text); { String.concat "" xs }

let arg :=
| braces(textual_expr)
| located_str = locate(VERBATIM);
  { [{located_str with value = Code.Verbatim located_str.value}] }

let txt_arg == braces(wstext)
let fun_spec == ~ = ident; ~ = binder; ~ = arg; <>

let main :=
| ~ = ws_list(locate(head_node)); EOF; <>
