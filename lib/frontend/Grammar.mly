%{
  open Forester_prelude
  open Forester_core
%}

%token <string> DECL_XMLNS
%token <string> TEXT VERBATIM
%token <string> WHITESPACE
%token <string> BSLASH_IDENT
%token BSLASH_LANGLE
%token IMPORT EXPORT DEF NAMESPACE LET OPEN
%token OBJECT PATCH CALL
%token SLASH HASH DOLLAR COMMA SEMI COLON
%token SUBTREE SCOPE PUT GET DEFAULT ALLOC
%token LBRACE RBRACE LANGLE RANGLE LSQUARE RSQUARE LPAREN RPAREN HASH_LBRACE HASH_HASH_LBRACE
%token QUERY_AND QUERY_OR QUERY_NOT QUERY_AUTHOR QUERY_TAG QUERY_TAXON QUERY_META
%token QUERY_TREE
%token EOF

%start <Code.t> main

%%

let locate(p) ==
| x = p; { Asai.Range.locate_lex $loc x }

let cons(p, q) ==
| x = p; xs = q; { x :: xs }

let braces(p) := delimited(LBRACE, p, RBRACE)
let squares(p) := delimited(LSQUARE, p, RSQUARE)
let parens(p) := delimited(LPAREN, p, RPAREN)

let bvar :=
| x = TEXT; { [x] }

let binder := list(squares(bvar))

let ws_or(p) :=
| WHITESPACE; { [] }
| x = p; { [x] }

let ws_list(p) := flatten(list(ws_or(p)))

let code_expr == ws_list(locate(head_node))

let text_or_ws :=
| TEXT
| WHITESPACE

let symbol_safe :=
| DOLLAR; { "$" }
| COMMA; { "," }
| SEMI; { ";" }
| COLON; { ":" }
| LANGLE; { "<" }
| RANGLE; { ">" }

// these symbols are used for hierarchical names and method call chains, so they can't appear
// directly after an "unsafe" head.
let symbol_unsafe :=
| SLASH; { "/" }
| HASH; { "#" }

let as_text_node(p) ==
| ~ = p; <Code.Text>

let as_verbatim_node(p) ==
| ~ = p; <Code.Verbatim>

let body :=
| body1
| cons(locate(as_text_node(symbol_unsafe)), body)

let body1 :=
| cons(locate(as_text_node(text_or_ws)), body)
| cons(locate(as_text_node(symbol_safe)), body)
| cons(locate(head_node_safe), body)
| cons(locate(head_node_unsafe), body1)
| { [] }


let head_node_safe :=
| DEF; (~,~,~) = fun_spec; <Code.Def>
| NAMESPACE; ~ = namespaced_ident; ~ = braces(code_expr); <Code.Namespace>
| SUBTREE; ~ = option(squares(wstext)); ~ = braces(body); <Code.Subtree>
| IMPORT; ~ = txt_arg; <Code.import_private>
| EXPORT; ~ = txt_arg; <Code.import_public>
| LET; (~,~,~) = fun_spec; <Code.Let>
| SCOPE; ~ = arg; <Code.Scope>
| PUT; ~ = namespaced_ident; ~ = arg; <Code.Put>
| DEFAULT; ~ = namespaced_ident; ~ = arg; <Code.Default>
| QUERY_TREE; ~ = braces(query); <Code.Query>
| BSLASH_LANGLE; ~ = xml_qident; RANGLE; ~ = list(xml_attr); ~ = arg; <Code.Xml_tag>
| ~ = DECL_XMLNS; ~ = txt_arg; <Code.Decl_xmlns>
| OBJECT; self = option(squares(bvar)); methods = braces(ws_list(method_decl)); { Code.Object {self; methods} }
| PATCH; obj = braces(code_expr); self = option(squares(bvar)); methods = braces(ws_list(method_decl)); { Code.Patch {obj; self; methods} }
| CALL; ~ = braces(code_expr); ~ = txt_arg; <Code.Call>
| ~ = delimited(HASH_LBRACE, body, RBRACE); <Code.inline_math>
| ~ = delimited(HASH_HASH_LBRACE, body, RBRACE); <Code.display_math>
| ~ = braces(body); <Code.braces>
| ~ = squares(body); <Code.squares>
| ~ = parens(body); <Code.parens>
| ~ = VERBATIM; <Code.Verbatim>

let xml_qident :=
| prefix = TEXT; COLON; name = TEXT; { Some prefix, name }
| name = TEXT; { None, name }

let xml_attr :=
  pair(squares(xml_qident), arg)

// These nodes have an undelimited backslashed identifier, and so we cannot allow
// arbitrary slashes or hashes after them due to potential ambiguity. For example,
// we want to interpret `\foo/bar` as an invocation to the hierarchical name `foo/bar`
// rather than an invocation to the name `foo` followed by the text `/bar`.
let head_node_unsafe :=
| ALLOC; ~ = namespaced_ident; <Code.Alloc>
| GET; ~ = namespaced_ident; <Code.Get>
| OPEN; ~ = namespaced_ident; <Code.Open>
| (~,~) = namespaced_ident_with_methods; <Code.Ident>

let head_node :=
| head_node_unsafe
| head_node_safe

let namespaced_ident :=
  cons(BSLASH_IDENT,loption(preceded(SLASH,separated_nonempty_list(SLASH, TEXT))))

let namespaced_ident_with_methods :=
  pair(namespaced_ident, loption(preceded(HASH,separated_nonempty_list(HASH,TEXT))))

let method_decl :=
  separated_pair(squares(TEXT), list(WHITESPACE), arg)

let query_node :=
| QUERY_AUTHOR; ~ = arg; <Query.Author>
| QUERY_TAG; ~ = arg; <Query.Tag>
| QUERY_TAXON; ~ = arg; <Query.Taxon>
| QUERY_AND; ~ = braces(queries); <Query.And>
| QUERY_OR; ~ = braces(queries); <Query.Or>
| QUERY_NOT; ~ = braces(query); <Query.Not>
| QUERY_META; k = txt_arg; v = arg; <Query.Meta>

let queries == ws_list(query_node)

let query :=
  delimited(list(WHITESPACE), query_node, list(WHITESPACE))

let ws_or_text :=
| TEXT
| WHITESPACE
| symbol_safe
| symbol_unsafe

let wstext :=
| xs = list(ws_or_text); { String.concat "" xs }


let arg :=
| braces(body)
| x = locate(as_verbatim_node(VERBATIM)); { [x] }


let txt_arg == braces(wstext)
let fun_spec == ~ = namespaced_ident; ~ = binder; ~ = arg; <>

let main :=
| ~ = body; EOF; <>
