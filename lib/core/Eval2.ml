open Forester_prelude

module T = Xml_tree2
module Q = Query

open Base

module V =
struct
  type t =
    | Content of T.content
    | Clo of t Env.t * Symbol.t binding list * Syn.t
    | Query_polarity of Q.polarity
    | Query_mode of Q.mode
    | Query_expr of Query.lnvar Query.expr
    | Sym of Symbol.t
    | Obj of Symbol.t
  [@@deriving show]

  type obj_method =
    {body : Syn.t;
     self : Symbol.t;
     super : Symbol.t;
     env : t Env.t}

  module MethodTable = Map.Make (String)

  type obj =
    {prototype : Symbol.t option;
     methods : obj_method MethodTable.t}

  type located = t Range.located

  let extract_content (node : located) =
    match node.value with
    | Content content -> content
    | _ -> Reporter.fatal ?loc:node.loc Type_error "Expected content"

  let coalesce_text =
    let rec loop acc =
      function
      | [] -> Option.some @@ String.concat "" @@ List.rev acc
      | T.Text txt :: content -> loop (txt :: acc) content
      | _ -> None
    in
    loop []


  let extract_text (node : located) =
    let content = extract_content node in
    match coalesce_text content with
    | Some txt -> String.trim txt
    | None -> Reporter.fatalf ?loc:node.loc Type_error "Expected address but got: %a" pp node.value

  let extract_query_polarity (x : located) =
    match x.value with
    | Query_polarity pol -> pol
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query polarity"

  let extract_query_mode (x : located) =
    match x.value with
    | Query_mode mode -> mode
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query mode"

  let extract_query_addr_expr (x : located) =
    match x.value with
    | Sym sym -> Query.Var (Query.F sym)
    | Content [Text txt] -> Query.Addr (User_addr txt)
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected addr expression in query"

  let extract_query_expr (x : located) =
    match x.value with
    | Query_expr q -> q
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected query expression"

  let extract_obj_ptr (x : located) =
    match x.value with
    | Obj sym -> sym
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected object"

  let extract_sym (x : located) =
    match x.value with
    | Sym sym -> sym
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected symbol"

end


module type I = sig
  val latex_to_svg : string -> string
end

module Make (I : I) =
struct
  module Tape = Tape_effect.Make ()
  module Lex_env = Algaeff.Reader.Make (struct type t = V.t Env.t end)
  module Dyn_env = Algaeff.Reader.Make (struct type t = V.t Env.t end)
  module Heap = Algaeff.State.Make (struct type t = V.obj Env.t end)
  module Emitted_trees = Algaeff.State.Make (struct type t = T.content T.article list end)
  module Frontmatter = Algaeff.State.Make (struct type t = T.content T.frontmatter end)
  module Scope = Algaeff.State.Make (Addr)

  let rec process_tape () =
    match Tape.pop_node_opt () with
    | None -> V.Content []
    | Some node -> eval_node node

  and eval_tape tape =
    Tape.run ~tape process_tape

  and eval_pop_arg ~loc =
    Tape.pop_arg ~loc
    |> Range.map eval_tape

  and pop_content_arg ~loc =
    eval_pop_arg ~loc
    |> V.extract_content

  and pop_text_arg ~loc =
    eval_pop_arg ~loc
    |> V.extract_text

  and eval_node node : V.t =
    let loc = node.loc in
    match node.value with
    | Var x ->
      eval_var ~loc x

    | Text str ->
      emit_content_node ~loc @@ T.Text str

    | Prim p ->
      let content = pop_content_arg ~loc |> T.trim_whitespace in
      emit_content_node ~loc @@ Prim (p, content)

    | Fun (xs, body) ->
      let env = Lex_env.read () in
      focus_clo env xs body

    | Ref ->
      let href = pop_text_arg ~loc in
      let addr = User_addr href in
      let content = [
        T.Transclude {addr; target = T.Taxon};
        T.Text " ";
        T.Transclude {addr; target = T.Number}
      ] in
      emit_content_node ~loc @@ Link {href; content}

    | Link {title; dest} ->
      let href = {node with value = dest} |> Range.map eval_tape |> V.extract_text in
      let content =
        match title with
        | None -> [T.Transclude {addr = User_addr href; target = T.Title}]
        | Some title -> {node with value = eval_tape title} |> V.extract_content
      in
      emit_content_node ~loc @@ Link {href; content}

    | Math (mode, body) ->
      let content =
        {node with value = eval_tape body} |> V.extract_content
      in
      emit_content_node ~loc @@ KaTeX (mode, content)

    | Xml_tag (name, attrs, body) ->
      let rec process : _ list -> T.xml_attr list =
        function
        | [] -> []
        | ((k : xml_resolved_qname), v) :: attrs ->
          let key = T.{prefix = k.prefix; uname = k.uname; xmlns = k.xmlns} in
          T.{key; value = V.extract_text {node with value = eval_tape v}} :: process attrs
      in
      let name = T.{prefix = name.prefix; uname = name.uname; xmlns = name.xmlns} in
      let content = {node with value = eval_tape body} |> V.extract_content in
      emit_content_node ~loc @@ T.Xml_elt {name; attrs = process attrs; content}

    | Query_polarity pol ->
      focus ?loc @@ V.Query_polarity pol

    | Query_mode mode ->
      focus ?loc @@ V.Query_mode mode

    | Query_rel ->
      let mode = eval_pop_arg ~loc |> V.extract_query_mode in
      let pol = eval_pop_arg ~loc |> V.extract_query_polarity in
      let rel = eval_pop_arg ~loc |> V.extract_text in
      let addr = eval_pop_arg ~loc |> V.extract_query_addr_expr in
      focus ?loc @@ V.Query_expr (Query.rel mode pol rel addr)

    | Query_isect ->
      let queries =
        Tape.pop_args () |> List.map @@ fun arg ->
        arg |> Range.map eval_tape |> V.extract_query_expr
      in
      focus ?loc @@ V.Query_expr (Query.isect queries)

    | Query_union ->
      let queries =
        Tape.pop_args () |> List.map @@ fun arg ->
        arg |> Range.map eval_tape |> V.extract_query_expr
      in
      focus ?loc @@ V.Query_expr (Query.union queries)

    | Query_compl ->
      let q = eval_pop_arg ~loc |> V.extract_query_expr in
      focus ?loc @@ V.Query_expr (Complement q)

    | Query_isect_fam ->
      let q = eval_pop_arg ~loc |> V.extract_query_expr in
      let qfun = Tape.pop_arg ~loc in
      let x = Symbol.fresh [] in
      let qx =
        let tape = qfun.value @ [{node with value = Syn.Sym x}] in
        {node with value = eval_tape tape} |> V.extract_query_expr
      in
      focus ?loc @@ V.Query_expr (Query.isect_fam q x qx)

    | Query_union_fam ->
      let q = eval_pop_arg ~loc |> V.extract_query_expr in
      let qfun = Tape.pop_arg ~loc in
      let x = Symbol.fresh [] in
      let qx =
        let tape = qfun.value @ [{node with value = Syn.Sym x}] in
        {node with value = eval_tape tape} |> V.extract_query_expr
      in
      focus ?loc @@ V.Query_expr (Query.union_fam q x qx)

    | Query_isect_fam_rel ->
      let q = eval_pop_arg ~loc |> V.extract_query_expr in
      let mode = eval_pop_arg ~loc |> V.extract_query_mode in
      let pol = eval_pop_arg ~loc |> V.extract_query_polarity in
      let rel = pop_text_arg ~loc in
      focus ?loc @@ V.Query_expr (Query.isect_fam_rel q mode pol rel)

    | Query_union_fam_rel ->
      let q = eval_pop_arg ~loc |> V.extract_query_expr in
      let mode = eval_pop_arg ~loc |> V.extract_query_mode in
      let pol = eval_pop_arg ~loc |> V.extract_query_polarity in
      let rel = pop_text_arg ~loc in
      focus ?loc @@ V.Query_expr (Query.union_fam_rel q mode pol rel)

    | Query_builtin builtin ->
      let addr = eval_pop_arg ~loc |> V.extract_query_addr_expr in
      let r =
        match builtin with
        | `Taxon -> Q.Rel.taxa
        | `Author -> Q.Rel.authors
        | `Tag -> Q.Rel.tags
      in
      let q = Query.rel Edges Incoming r addr in
      focus ?loc:node.loc @@ V.Query_expr q

    | TeX_cs cs ->
      emit_content_node ~loc @@ TeX_cs cs

    | Transclude ->
      let addr = User_addr (pop_text_arg ~loc) in
      let mainmatter = [T.Transclude {addr; target = Mainmatter}] in
      (* TODO: need to figure out how to apply transclusion options to unknown frontmatter *)
      emit_content_node ~loc @@ T.Transclude {addr; target = Full}

    | Subtree (addr_opt, nodes) ->
      let addr =
        match addr_opt with
        | Some addr -> User_addr addr
        | None -> Machine_addr (Oo.id (object end))
      in
      let subtree = eval_tree_inner ~addr nodes in
      let frontmatter = Frontmatter.get () in
      let subtree = {subtree with frontmatter = {subtree.frontmatter with physical_parent = Some frontmatter.addr; designated_parent = Some frontmatter.addr}} in
      Emitted_trees.modify @@ List.cons subtree;
      emit_content_node ~loc @@ Transclude {addr; target = Full}

    | Query_tree ->
      let query =
        eval_pop_arg ~loc
        |> V.extract_query_expr
        |> Query.distill_expr
      in
      emit_content_node ~loc @@ Results_of_query query

    | Embed_tex ->
      let svg_code =
        let preamble = pop_content_arg ~loc in
        let source = pop_content_arg ~loc in
        Reporter.with_loc loc @@ fun () ->
        I.latex_to_svg @@ Format.asprintf "%a\n\n%a" T.TeX_like.pp_content preamble T.TeX_like.pp_content source
      in

      let format = "svg+xml" in
      let base64 = Base64.encode_string svg_code in
      emit_content_node ~loc @@ Img (Inline {format; base64})

    | Object {self; methods} ->
      let table =
        let env = Lex_env.read () in
        let add (name, body) =
          let super = Symbol.fresh [] in
          V.MethodTable.add name V.{body; self; super; env}
        in
        List.fold_right add methods V.MethodTable.empty
      in
      let sym = Symbol.fresh ["obj"] in
      Heap.modify @@ Env.add sym V.{prototype = None; methods = table};
      focus ?loc:node.loc @@ V.Obj sym

    | Patch {obj; self; super; methods} ->
      let obj_ptr = {node with value = obj} |> Range.map eval_tape |> V.extract_obj_ptr in
      let table =
        let env = Lex_env.read () in
        let add (name, body) =
          V.MethodTable.add name
            V.{body; self; super; env}
        in
        List.fold_right add methods V.MethodTable.empty
      in
      let sym = Symbol.fresh ["obj"] in
      Heap.modify @@ Env.add sym V.{prototype = Some obj_ptr; methods = table};
      focus ?loc:node.loc @@ V.Obj sym

    | Group (d, body) ->
      let l, r = Base.delim_to_strings d in
      let content =
        T.Text l
        :: V.extract_content {node with value = eval_tape body}
        @ [T.Text r]
      in
      focus ?loc:node.loc @@ V.Content content

    | Call (obj, method_name) ->
      let sym = {node with value = obj} |> Range.map eval_tape |> V.extract_obj_ptr in
      let rec call_method (obj : V.obj) =
        let proto_val =
          obj.prototype |> Option.map @@ fun ptr ->
          V.Obj ptr
        in
        match V.MethodTable.find_opt method_name obj.methods with
        | Some mthd ->
          let env =
            let env = Env.add mthd.self (V.Obj sym) mthd.env in
            match proto_val with
            | None -> env
            | Some proto_val ->
              Env.add mthd.super proto_val env
          in
          Lex_env.run ~env @@ fun () ->
          eval_tape mthd.body
        | None ->
          match obj.prototype with
          | Some proto ->
            call_method @@ Env.find proto @@ Heap.get ()
          | None ->
            Reporter.fatalf ?loc:node.loc Type_error
              "tried to call unbound method `%s`" method_name
      in
      let result = call_method @@ Env.find sym @@ Heap.get () in
      focus ?loc:node.loc result

    | Put (k, v, body) ->
      let k = {node with value = k} |> Range.map eval_tape |> V.extract_sym in
      let body =
        Dyn_env.scope (Env.add k @@ eval_tape v) @@ fun () ->
        eval_tape body
      in
      focus ?loc:node.loc body

    | Default (k, v, body) ->
      let k = {node with value = k} |> Range.map eval_tape |> V.extract_sym in
      let body =
        let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval_tape v) flenv in
        Dyn_env.scope upd @@ fun () ->
        eval_tape body
      in
      focus ?loc:node.loc body

    | Get k ->
      let k = {node with value = k} |> Range.map eval_tape |> V.extract_sym in
      let env = Dyn_env.read () in
      begin
        match Env.find_opt k env with
        | None ->
          Eio.traceln "getting %a from %a" Symbol.pp k (Env.pp V.pp) env;
          Reporter.fatalf ?loc:node.loc Resolution_error
            "could not find fluid binding named %a"
            Symbol.pp k
        | Some v -> focus ?loc:node.loc v
      end

    | Verbatim str ->
      emit_content_node ~loc @@ CDATA str

    | Title ->
      let title = pop_content_arg ~loc in
      Frontmatter.modify (fun fm -> {fm with title = title});
      process_tape ()

    | Parent ->
      let addr = pop_text_arg ~loc in
      Frontmatter.modify (fun fm -> {fm with designated_parent = Some (User_addr addr)});
      process_tape ()

    | Meta ->
      let k = pop_text_arg ~loc  in
      let v = pop_content_arg ~loc in
      Frontmatter.modify (fun fm -> {fm with metas = fm.metas @ [k,v]});
      process_tape ()

    | Author ->
      let author = pop_text_arg ~loc in
      Frontmatter.modify (fun fm -> {fm with attributions = fm.attributions @ [T.Author author]});
      process_tape ()

    | Contributor ->
      let author = pop_text_arg ~loc in
      Frontmatter.modify (fun fm -> {fm with attributions = fm.attributions @ [T.Contributor author]});
      process_tape ()

    | Tag ->
      let tag = pop_text_arg ~loc in
      let scope = Scope.get () in
      Frontmatter.modify (fun fm -> {fm with tags = fm.tags @ [tag]});
      process_tape ()

    | Date ->
      let date_str = pop_text_arg ~loc in
      begin
        match Date.parse date_str with
        | None ->
          Reporter.fatalf ?loc:node.loc Parse_error "Invalid date string `%s`" date_str
        | Some date ->
          Frontmatter.modify (fun fm -> {fm with dates = fm.dates @ [date]});
          process_tape ()
      end

    | Number ->
      let num = pop_text_arg ~loc in
      Frontmatter.modify (fun fm -> {fm with number = Some num});
      process_tape ()

    | Taxon ->
      let taxon = pop_text_arg ~loc in
      let scope = Scope.get () in
      Frontmatter.modify (fun fm -> {fm with taxon = Some taxon});
      process_tape ()

    | Sym sym ->
      focus ?loc:node.loc @@ V.Sym sym

  and eval_var ~loc x =
    match Env.find_opt x @@ Lex_env.read () with
    | Some v -> focus ?loc v
    | None ->
      Reporter.fatalf ?loc Resolution_error
        "could not find variable named %a"
        Symbol.pp x

  and focus ?loc =
    function
    | V.Clo (rho, xs, body) ->
      focus_clo ?loc rho xs body

    | V.Content content ->
      begin
        match process_tape () with
        | V.Content content' -> V.Content (content @ content')
        | value -> value
      end

    | V.Query_expr _ | V.Query_mode _ | V.Query_polarity _ | V.Sym _ | V.Obj _ as v ->
      begin
        match process_tape () with
        | V.Content content when T.strip_whitespace content = [] -> v
        | _ -> Reporter.fatalf ?loc Type_error "Expected solitary node"
      end

  and focus_clo ?loc rho xs body =
    match xs with
    | [] ->
      focus ?loc @@
      Lex_env.run ~env:rho @@ fun () ->
      eval_tape body
    | (strategy, y) :: ys ->
      match Tape.pop_arg_opt () with
      | Some arg ->
        let yval =
          match strategy with
          | Strict -> eval_tape arg.value
          | Lazy -> V.Clo (Lex_env.read (), [(Strict, Symbol.fresh [])], arg.value)
        in
        let rhoy = Env.add y yval rho in
        focus_clo ?loc rhoy ys body
      | None ->
        begin
          match process_tape () with
          | Content nodes when T.strip_whitespace nodes = [] ->
            Clo (rho, xs, body)
          | _ ->
            Reporter.fatalf ?loc Type_error "Expected %i additional arguments" (List.length xs)
        end

  and emit_content_node ~loc content =
    focus ?loc @@ Content [content]


  and eval_tree_inner ~addr (tree : Syn.tree) : T.content T.article =
    let scope =
      match addr with
      | User_addr _ -> addr
      | _ -> Scope.get ()
    in
    Scope.run ~init:scope @@ fun () ->
    let outer_frontmatter = Frontmatter.get () in
    let frontmatter =
      {(T.empty_frontmatter ~addr) with
       source_path = outer_frontmatter.source_path;
       attributions = outer_frontmatter.attributions;
       dates = outer_frontmatter.dates}
    in
    Frontmatter.run ~init:frontmatter @@ fun () ->
    let mainmatter = {value = eval_tape tree; loc = None} |> V.extract_content in
    let frontmatter = Frontmatter.get () in
    let backmatter = T.default_backmatter ~addr in
    T.{frontmatter; mainmatter; backmatter}

  let eval_tree ~addr ~source_path (tree : Syn.tree) : T.content T.article * T.content T.article list =
    let fm = {(T.empty_frontmatter ~addr) with source_path} in
    Frontmatter.run ~init:fm @@ fun () ->
    Scope.run ~init:addr @@ fun () ->
    Emitted_trees.run ~init:[] @@ fun () ->
    Heap.run ~init:Env.empty @@ fun () ->
    Lex_env.run ~env:Env.empty @@ fun () ->
    Dyn_env.run ~env:Env.empty @@ fun () ->
    let tree = eval_tree_inner ~addr tree in
    let emitted = Emitted_trees.get () in
    tree, emitted

end