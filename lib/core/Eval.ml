open Forester_prelude
open Base
open Bwd

module Q = Query



module G =
struct
  module G = Graph.Imperative.Digraph.Concrete (Addr)
  include G
  include Graph.Oper.I (G)

  let safe_succ g x =
    if mem_vertex g x then succ g x else []

  let safe_fold_succ f g x acc =
    if mem_vertex g x then
      fold_succ f g x acc
    else
      acc

  let safe_pred g x =
    if mem_vertex g x then pred g x else []
end

module Make () =
struct
  let refresher_queue = Queue.create ()

  let refresh trees =
    refresher_queue |> Queue.iter @@ fun f ->
    f trees

  let observe f =
    Queue.add f refresher_queue

  module Graphs =
  struct
    let all_addrs_ref : Addr_set.t ref =
      ref Addr_set.empty

    let rel_to_graph : (Q.Rel.t, G.t) Hashtbl.t =
      Hashtbl.create 20

    let rel_to_preorder : (Q.Rel.t, G.t) Hashtbl.t =
      Hashtbl.create 20

    let get_graph rel =
      match Hashtbl.find_opt rel_to_graph rel with
      | None ->
        let gph = G.create () in
        Hashtbl.add rel_to_graph rel gph;
        gph
      | Some gph -> gph

    let get_preorder rel =
      match Hashtbl.find_opt rel_to_preorder rel with
      | None ->
        let gph = G.transitive_closure ~reflexive:true @@ get_graph rel in
        Hashtbl.add rel_to_preorder rel gph;
        gph
      | Some gph -> gph

    let get (mode : Q.mode) =
      match mode with
      | Edges -> get_graph
      | Paths -> get_preorder

    let register_addr addr =
      Hashtbl.clear rel_to_preorder;
      all_addrs_ref := Addr_set.add addr !all_addrs_ref;
      rel_to_graph |> Hashtbl.iter @@ fun _ gph ->
      G.add_vertex gph addr

    let add_edge rel ~source ~target =
      Hashtbl.remove rel_to_preorder rel;
      let gph = get_graph rel in
      G.add_edge gph source target
  end

  module Query_engine =
  struct
    let query_rel mode pol rel addr =
      let fn =
        match pol with
        | Q.Incoming -> G.safe_pred
        | Q.Outgoing -> G.safe_succ
      in
      let gph = Graphs.get mode rel in
      Addr_set.of_list @@ fn gph addr

    let check_rel mode pol rel addr addr' =
      let gph = Graphs.get mode rel in
      match pol with
      | Q.Incoming -> G.mem_edge gph addr' addr
      | Q.Outgoing -> G.mem_edge gph addr addr'

    let rec check_query q addr =
      match Q.view q with
      | Q.Rel ((mode, pol, rel), addr') ->
        check_rel mode pol rel addr' addr
      | Q.Isect qs -> check_isect qs addr
      | Q.Union qs -> check_union qs addr
      | Q.Complement q ->
        not @@ check_query q addr
      | Q.Isect_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        xs |> List.for_all @@ fun x ->
        check_rel mode pol rel x addr
      | Q.Union_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        xs |> List.exists @@ fun x ->
        check_rel mode pol rel x addr

    and check_isect qs addr =
      qs |> List.for_all @@ fun q ->
      check_query q addr

    and check_union qs addr =
      qs |> List.exists @@ fun q ->
      check_query q addr


    and run_query q =
      match Q.view q with
      | Q.Rel ((mode, pol, rel), addr) ->
        query_rel mode pol rel addr
      | Q.Isect qs -> run_isect qs
      | Q.Union qs -> run_union qs
      | Q.Complement q ->
        Addr_set.diff !Graphs.all_addrs_ref @@ run_query q
      | Q.Isect_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        run_isect @@ List.map (Q.rel mode pol rel) xs
      | Q.Union_fam (q, (mode, pol, rel)) ->
        let xs = Addr_set.to_list @@ run_query q in
        run_union @@ List.map (Q.rel mode pol rel) xs

    and run_isect =
      function
      | [] -> !Graphs.all_addrs_ref
      | q :: qs ->
        run_query q |> Addr_set.filter @@ check_isect qs

    and run_union qs =
      let alg q = Addr_set.union (run_query q) in
      List.fold_right alg qs Addr_set.empty

    and fold_set_operation opr running =
      function
      | [] -> running
      | q :: qs ->
        let s = run_query q in
        fold_set_operation opr (opr running s) qs
  end

  module Lex_env = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
  module Dyn_env = Algaeff.Reader.Make (struct type t = Sem.t Env.t end)
  module Heap = Algaeff.State.Make (struct type t = Sem.obj Env.t end)
  module Emitted_trees = Algaeff.State.Make (struct type t = Sem.tree list end)
  module Fm = Algaeff.State.Make (struct type t = Sem.frontmatter end)
  module Scope = Algaeff.State.Make (Addr)

  let get_transclusion_opts () =
    let dynenv = Dyn_env.read () in
    let title_override = Env.find_opt Expand.Builtins.Transclude.title_sym dynenv in
    let taxon_override =
      match Env.find_opt Expand.Builtins.Transclude.taxon_sym dynenv with
      | Some [{value = Sem.Text text; _}] -> Some text
      | _ -> None
    in
    let get_bool key default =
      match Env.find_opt key dynenv with
      | Some [{value = Sem.Text "true"; _}] -> true
      | Some [{value = Sem.Text "false"; _}] -> false
      | _ -> default
    in
    let expanded = get_bool Expand.Builtins.Transclude.expanded_sym true in
    let show_heading = get_bool Expand.Builtins.Transclude.show_heading_sym true in
    let toc = get_bool Expand.Builtins.Transclude.toc_sym true in
    let numbered = get_bool Expand.Builtins.Transclude.numbered_sym true in
    let show_metadata = get_bool Expand.Builtins.Transclude.show_metadata_sym false in
    Sem.{title_override; taxon_override; toc; show_heading; expanded; numbered; show_metadata}

  let pop_arg_opt rest =
    match rest with
    | Range.{value = Syn.Group (Braces, arg); _} as node :: rest ->
      Some ({node with value = arg}, rest)
    | Range.{value = (Syn.Sym _ | Syn.Verbatim _ | Syn.Var _); _} as node :: rest ->
      Some ({node with value = [node]}, rest)
    | _ -> None


  let pop_arg ~loc rest =
    match pop_arg_opt rest with
    | Some (arg, rest) -> arg, rest
    | None ->
      Reporter.fatalf ?loc Type_error "Expected argument"


  type value =
    | VContent of Xml_tree.content_
    | VClo of value Env.t * Symbol.t binding list * Syn.t

  module Lex_env' = Algaeff.Reader.Make (struct type t = value Env.t end)

  let rec eval =
    function
    | [] -> VContent []
    | node :: nodes ->
      eval_node node nodes

  and eval_node (node : Syn.node Range.located) nodes =
    match node.value with
    | Syn.Text str ->
      emit_content_node (Xml_tree.Text str) nodes

    | Syn.Prim p ->
      let arg, nodes = pop_arg ~loc:node.loc nodes in
      let content =
        eval_as_content ?loc:arg.loc arg.value
        |> Xml_tree.trim_whitespace
      in
      emit_content_node (Xml_tree.Prim (p, content)) nodes

    | Syn.Fun (xs, body) ->
      let env = Lex_env'.read () in
      focus_clo env xs body nodes

    | Ref ->
      let dest, nodes = pop_arg ~loc:node.loc nodes in
      let scope = Scope.get () in
      let dest = eval_as_addr ?loc:dest.loc dest.value in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:dest;
      let ref =
        Xml_tree.{
          addr = dest;
          taxon = None;
          number = None;
        }
      in
      observe begin fun trees ->
        Addr_map.find_opt dest trees |> Option.iter @@ function
        | Xml_tree.Tree tree ->
          ref.number <- tree.frontmatter.number;
          ref.taxon <- tree.frontmatter.taxon
      end;
      emit_content_node (Xml_tree.Ref ref) nodes

    | Link {title; dest} ->
      let scope = Scope.get () in
      let dest = Xml_tree.extract_text @@ eval_as_content ?loc:node.loc dest in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:(User_addr dest);
      let title = Option.map (eval_as_content ?loc:node.loc) title in
      let splice = Xml_tree.{splice = Content []} in
      observe begin fun trees ->
        splice.splice <-
          match Addr_map.find_opt (User_addr dest) trees with
          | None ->
            let content =
              match title with
              | None -> Xml_tree.Content [Xml_tree.Text "Untitled"]
              | Some title -> title
            in
            let title = Option.map Xml_tree.extract_text title in
            let link = Xml_tree.{href = dest; content; title} in
            Xml_tree.Content [External_link link]
          | Some (Xml_tree.Tree tree) ->
            let content =
              match title with
              | Some title -> title
              | None ->
                match tree.frontmatter.title with
                | None -> Xml_tree.Content [Xml_tree.Text (Format.sprintf "[%s]" dest)]
                | Some title -> title
            in
            let title = Option.map Xml_tree.extract_text tree.frontmatter.title in
            let link = Xml_tree.{addr = User_addr dest; content; title} in
            Xml_tree.Content [Local_link link]
      end;
      emit_content_node (Xml_tree.Splice splice) nodes

    | Math (mmode, e) ->
      (* {node with value = Sem.Math (mmode, eval_as_content ?loc:node.loc e)}  *)
      emit_content_node (Xml_tree.TeX (failwith "")) nodes

    | _ -> failwith ""


  and eval_as_content ?loc arg =
    match eval arg with
    | VContent content -> Xml_tree.Content content
    | _ -> Reporter.fatalf ?loc Type_error "Expected content"

  and eval_as_addr ?loc arg =
    let content = eval_as_content ?loc arg in
    User_addr (failwith "")

  and focus v nodes =
    match v with
    | VContent content ->
      emit_content content nodes

    | VClo (rho, xs, body) ->
      focus_clo rho xs body nodes

  and focus_clo rho xs body nodes =
    match xs with
    | [] ->
      focus (Lex_env'.run ~env:rho @@ fun () -> eval body) nodes
    | Strict y :: ys ->
      match pop_arg_opt nodes with
      | Some (arg, nodes) ->
        let rhoy = Env.add y (eval arg.value) rho in
        focus_clo rhoy ys body nodes
      | None ->
        begin
          match eval nodes with
          | VContent (* nodes when Sem.strip_whitespace nodes = *) [] ->
            VClo (rho, xs, body)
          | _ -> Reporter.fatalf Type_error "foo"
        end

  and emit_content content nodes =
    match eval nodes with
    | VContent content' -> VContent (content @ content')
    | _ -> Reporter.fatalf Type_error "focus_content"

  and emit_content_node content nodes =
    emit_content [content] nodes








  let sem_pop_arg_opt rest =
    match rest with
    | Range.{value = Sem.Group (Braces, arg); _} as node :: rest ->
      Some ({node with value = arg}, rest)
    | Range.{value = (Sem.Sym _ | Sem.Verbatim _); _} as node :: rest ->
      Some ({node with value = [node]}, rest)
    | _ -> None

  let pop_arg ~loc rest =
    match pop_arg_opt rest with
    | Some (arg, rest) -> arg, rest
    | None ->
      Reporter.fatalf ?loc Type_error "Expected argument"

  let pop_args rest =
    let rec loop acc rest =
      match pop_arg_opt rest with
      | Some (arg, rest) -> loop (Bwd.Snoc (acc, arg)) rest
      | None -> Bwd.prepend acc [], rest
    in
    loop Bwd.Emp rest

  let rec eval : Syn.t -> Sem.t =
    function
    | [] -> []
    | node :: rest ->
      eval_node node rest

  and eval_node : Syn.node Range.located -> Syn.t -> Sem.t =
    fun node rest ->
    match node.value with
    | Link {title; dest} ->
      let scope = Scope.get () in
      let dest = eval_addr {node with value = dest} in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:dest;
      let title = Option.map eval title in
      {node with value = Sem.Link (dest, title, Identity)} >> eval rest

    | Ref ->
      let dest, rest = pop_arg ~loc:node.loc rest in
      let scope = Scope.get () in
      let dest = eval_addr dest in
      Graphs.add_edge Q.Rel.links ~source:scope ~target:dest;
      {node with value = Sem.Ref dest} >> eval rest

    | Query_polarity pol ->
      {node with value = Sem.Query_polarity pol} >> eval rest

    | Query_mode mode ->
      {node with value = Sem.Query_mode mode} >> eval rest

    | Math (mmode, e) ->
      {node with value = Sem.Math (mmode, eval e)} >> eval rest

    | Prim p ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      {node with value = Sem.Prim (p, eval_trim arg.value)} >> eval rest

    | Xml_tag (name, attrs, body) ->
      let rec process attrs = match attrs with
        | [] -> []
        | (k,v) :: attrs ->
          let processed = process attrs in
          if List.mem_assoc k processed then begin
            Reporter.emitf ?loc:node.loc Duplicate_attribute
              "skipping duplicate XML attribute `%a`" pp_xml_resolved_qname k;
            processed
          end else
            (k, eval v) :: processed
      in
      {node with value = Sem.Xml_tag (name, process attrs, eval body)} >> eval rest

    | TeX_cs cs ->
      {node with value = Sem.TeX_cs cs} >> eval rest

    | Transclude ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:addr;
      let opts = get_transclusion_opts () in
      {node with value = Sem.Transclude (opts, addr)} >> eval rest

    | Subtree (addr, nodes) ->
      let addr =
        match addr with
        | Some addr -> User_addr addr
        | None -> Machine_addr (Oo.id (object end))
      in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:addr;
      let opts = get_transclusion_opts () in
      let subtree = eval_tree_inner ~addr nodes in
      let fm = Fm.get () in
      let subtree = {subtree with fm = {subtree.fm with physical_parent = Some fm.addr; designated_parent = Some fm.addr}} in
      begin
        Emitted_trees.modify @@ fun trees ->
        subtree :: trees
      end;
      {node with value = Sem.Subtree (opts, subtree)} >> eval rest

    | Query_tree ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let opts = get_transclusion_opts () in
      let opts =
        match opts.title_override with
        | None -> {opts with show_heading = false; toc = false}
        | Some _ -> opts
      in
      let query = eval_query_expr arg in
      {node with value = Sem.Query_tree (opts, query)} >> eval rest

    | Embed_tex ->
      let preamble, rest = pop_arg ~loc:node.loc rest in
      let source, rest = pop_arg ~loc:node.loc rest in
      {node with value = Sem.Embed_tex {preamble = eval preamble.value; source = eval source.value}} >> eval rest

    | Fun (xs, body) ->
      let env = Lex_env.read () in
      {node with value = Sem.Clo (env, xs, body)} >> eval rest

    | Object {self; methods} ->
      let table =
        let env = Lex_env.read () in
        let add (name, body) =
          let super = Symbol.fresh [] in
          Sem.MethodTable.add name Sem.{body; self; super; env}
        in
        List.fold_right add methods Sem.MethodTable.empty
      in
      let sym = Symbol.fresh ["obj"] in
      Heap.modify @@ Env.add sym Sem.{prototype = None; methods = table};
      {node with value = Sem.Object sym} >> eval rest

    | Patch {obj; self; super; methods} ->
      begin
        match eval_strip obj with
        | [Range.{value = Sem.Object obj_ptr; _}] ->
          let table =
            let env = Lex_env.read () in
            let add (name, body) =
              Sem.MethodTable.add name
                Sem.{body; self; super; env}
            in
            List.fold_right add methods Sem.MethodTable.empty
          in
          let sym = Symbol.fresh ["obj"] in
          Heap.modify @@ Env.add sym Sem.{prototype = Some obj_ptr; methods = table};
          {node with value = Sem.Object sym} >> eval rest
        | xs ->
          Reporter.fatalf ?loc:node.loc Type_error
            "tried to patch non-object"
      end

    | Call (obj, method_name) ->
      begin
        match eval_strip obj with
        | [Range.{value = Sem.Object sym; _}] as obj_val ->
          let rec call_method (obj : Sem.obj) =
            let proto_val =
              obj.prototype |> Option.map @@ fun ptr ->
              [Range.locate_opt None @@ Sem.Object ptr]
            in
            match Sem.MethodTable.find_opt method_name obj.methods with
            | Some mthd ->
              let env =
                let env = Env.add mthd.self obj_val mthd.env in
                match proto_val with
                | None -> env
                | Some proto_val ->
                  Env.add mthd.super proto_val env
              in
              Lex_env.scope (fun _ -> env) @@ fun () ->
              eval mthd.body
            | None ->
              match obj.prototype with
              | Some proto ->
                call_method @@ Env.find proto @@ Heap.get ()
              | None ->
                Reporter.fatalf ?loc:node.loc Type_error
                  "tried to call unbound method `%s`" method_name
          in
          let result = call_method @@ Env.find sym @@ Heap.get () in
          result >>* eval rest
        | xs ->
          Reporter.fatalf ?loc:node.loc Type_error
            "tried to call method `%s` on non-object: %a" method_name Sem.pp xs
      end

    | Var x ->
      begin
        match Env.find_opt x @@ Lex_env.read () with
        | None ->
          Reporter.fatalf ?loc:node.loc Resolution_error
            "could not find variable named %a"
            Symbol.pp x
        | Some v -> v >>* eval rest
      end

    | Put (k, v, body) ->
      let k = eval_sym {node with value = k} in
      let body =
        Dyn_env.scope (Env.add k @@ eval v) @@ fun () ->
        eval body
      in
      body >>* eval rest

    | Default (k, v, body) ->
      let k = eval_sym {node with value = k} in
      let body =
        let upd flenv = if Env.mem k flenv then flenv else Env.add k (eval v) flenv in
        Dyn_env.scope upd @@ fun () ->
        eval body
      in
      body >>* eval rest

    | Get key ->
      let key = eval_sym {node with value = key} in
      begin
        let env = Dyn_env.read () in
        match Env.find_opt key env with
        | None ->
          Eio.traceln "getting %a from %a" Symbol.pp key (Env.pp Sem.pp) env;
          Reporter.fatalf ?loc:node.loc Resolution_error
            "could not find fluid binding named %a"
            Symbol.pp key
        | Some v -> v >>* eval rest
      end

    | Verbatim str ->
      {node with value = Sem.Verbatim str} >> eval rest

    | Text str ->
      {node with value = Sem.Text str} >> eval rest

    | Group (d, xs) ->
      {node with value = Sem.Group (d, eval xs)} >> eval rest

    | Title ->
      let title, rest = pop_arg ~loc:node.loc rest in
      let title = eval title.value in
      Fm.modify (fun fm -> {fm with title = Some title});
      eval rest

    | Parent ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      Fm.modify (fun fm -> {fm with designated_parent = Some addr});
      eval rest

    | Meta ->
      let argk, rest = pop_arg ~loc:node.loc rest in
      let argv, rest = pop_arg ~loc:node.loc rest in
      let k = eval_as_string argk in
      let v = eval argv.value in
      Fm.modify (fun fm -> {fm with metas = fm.metas @ [k,v]});
      eval rest

    | Author ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.authors ~source:scope ~target:addr;
      Fm.modify (fun fm -> {fm with authors = fm.authors @ [addr]});
      eval rest

    | Contributor ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.contributors ~source:scope ~target:addr;
      Fm.modify (fun fm -> {fm with contributors = fm.contributors @ [addr]});
      eval rest

    | Tag ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let tag = eval_as_string arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.tags ~source:scope ~target:(User_addr tag);
      Fm.modify (fun fm -> {fm with tags = fm.tags @ [tag]});
      eval rest

    | Date ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let date = eval_as_string arg in
      begin
        match Date.parse date with
        | None ->
          Reporter.fatalf Parse_error "Invalid date string `%s`" date
        | Some date ->
          Fm.modify (fun fm -> {fm with dates = fm.dates @ [date]});
          eval rest
      end

    | Number ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let num = eval_as_string arg in
      Fm.modify (fun fm -> {fm with number = Some num});
      eval rest

    | Taxon ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let taxon = eval_as_string arg in
      let scope = Scope.get () in
      Graphs.add_edge Q.Rel.taxa ~source:scope ~target:(User_addr taxon);
      Fm.modify (fun fm -> {fm with taxon = Some taxon});
      eval rest

    | Query_rel ->
      let arg_mode, rest = pop_arg ~loc:node.loc rest in
      let arg_pol, rest = pop_arg ~loc:node.loc rest in
      let arg_sym, rest = pop_arg ~loc:node.loc rest in
      let arg_addr, rest = pop_arg ~loc:node.loc rest in
      let mode = eval_query_mode arg_mode in
      let pol = eval_query_polarity arg_pol in
      let sym = eval_sym arg_sym in
      let addr = eval_addr arg_addr in
      {node with value = Sem.Query (Q.rel mode pol sym addr)} >> eval rest

    | Query_isect ->
      let args, rest = pop_args rest in
      let args = List.map eval_query_expr args in
      {node with value = Sem.Query (Q.isect args)} >> eval rest

    | Query_union ->
      let args, rest = pop_args rest in
      let args = List.map eval_query_expr args in
      {node with value = Sem.Query (Q.union args)} >> eval rest

    | Query_compl ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let q = eval_query_expr arg in
      {node with value = Sem.Query (Q.complement q)} >> eval rest

    | Query_isect_fam ->
      let argq, rest = pop_arg ~loc:node.loc rest in
      let arg_mode, rest = pop_arg ~loc:node.loc rest in
      let arg_pol, rest = pop_arg ~loc:node.loc rest in
      let arg_sym, rest = pop_arg ~loc:node.loc rest in
      let q = eval_query_expr argq in
      let mode = eval_query_mode arg_mode in
      let pol = eval_query_polarity arg_pol in
      let sym = eval_sym arg_sym in
      {node with value = Sem.Query (Q.isect_fam q mode pol sym)} >> eval rest

    | Query_union_fam ->
      let argq, rest = pop_arg ~loc:node.loc rest in
      let arg_mode, rest = pop_arg ~loc:node.loc rest in
      let arg_pol, rest = pop_arg ~loc:node.loc rest in
      let arg_sym, rest = pop_arg ~loc:node.loc rest in
      let q = eval_query_expr argq in
      let mode = eval_query_mode arg_mode in
      let pol = eval_query_polarity arg_pol in
      let sym = eval_sym arg_sym in
      {node with value = Sem.Query (Q.union_fam q mode pol sym)} >> eval rest

    | Query_builtin builtin ->
      let arg, rest = pop_arg ~loc:node.loc rest in
      let addr = eval_addr arg in
      let q =
        match builtin with
        | `Taxon -> Q.rel Edges Incoming Q.Rel.taxa addr
        | `Author -> Q.rel Edges Incoming Q.Rel.authors addr
        | `Tag -> Q.rel Edges Incoming Q.Rel.tags addr
      in
      {node with value = Sem.Query q} >> eval rest

    | Sym sym ->
      {node with value = Sem.Sym sym} >> eval rest

  and (>>*) nodes rest =
    match nodes with
    | [] -> rest
    | [x] -> x >> rest
    | _ -> nodes @ rest

  and (>>) node (rest : Sem.t)  =
    match Range.(node.value) with
    | Clo (env, xs, body) ->
      let rec loop env xs rest =
        match xs with
        | [] ->
          let result =
            Lex_env.run ~env @@ fun () ->
            eval body
          in
          begin
            result >>* rest
          end
        | (Strict x :: xs) as ys ->
          begin
            match sem_pop_arg_opt rest with
            | Some (u, rest) ->
              loop (Env.add x u.value env) xs rest
            | None ->
              begin
                match Sem.strip_whitespace rest with
                | [] ->
                  let clo = Range.locate_opt node.loc @@ Sem.Clo (env, ys, body) in
                  clo :: rest
                | _ ->
                  Reporter.fatalf ?loc:node.loc Type_error "Expected additional arguments"
              end
          end
      in
      loop env xs rest
    | _ ->
      node :: rest

  and eval_query_polarity x =
    extract_query_polarity {x with value = eval x.value}

  and eval_query_mode x =
    extract_query_mode {x with value = eval x.value}

  and extract_query_mode x =
    match Sem.strip_whitespace x.value with
    | [{value = Query_mode mode; _}] -> mode
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected polarity in query expression"

  and extract_query_polarity x =
    match Sem.strip_whitespace x.value with
    | [{value = Query_polarity pol; _}] -> pol
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected polarity in query expression"


  and eval_strip xs = Sem.strip_whitespace @@ eval xs

  and eval_trim xs = Sem.trim_whitespace @@ eval xs

  and eval_sym (x : Syn.t Range.located)  =
    extract_sym {x with value = eval x.value}

  and extract_sym x =
    match Sem.strip_whitespace x.value with
    | [{value = Sem.Sym sym; _}] -> sym
    | _ -> Reporter.fatalf ?loc:x.loc Type_error "Expected symbol here"


  and eval_query_expr node =
    extract_query_expr @@ {node with value = eval node.value}

  and extract_query_expr (x : Sem.t Range.located) =
    match Sem.strip_whitespace x.value with
    | [Range.{value = Sem.Query q; _}] -> q
    | u -> Reporter.fatalf ?loc:x.loc Type_error "Failed to evaluate query expression, got %a" Sem.pp u


  and eval_as_string x =
    Sem.string_of_nodes @@ eval x.value

  and eval_addr x =
    User_addr (eval_as_string x)

  and to_addr (x : Sem.t) =
    User_addr (Sem.string_of_nodes x)

  and eval_tree_inner ~addr (tree : Syn.tree) : Sem.tree =
    Graphs.register_addr addr;
    let scope =
      match addr with
      | User_addr _ -> addr
      | _ -> Scope.get ()
    in
    Scope.run ~init:scope @@ fun () ->
    let outer_fm = Fm.get () in
    let fm =
      {(Sem.empty_frontmatter ~addr) with
       source_path = outer_fm.source_path;
       authors = outer_fm.authors;
       dates = outer_fm.dates}
    in
    Fm.run ~init:fm @@ fun () ->
    let bm = Sem.default_backmatter ~addr in (*TODO*)
    let body = eval tree in
    let fm = Fm.get () in
    let open Sem in
    {fm; body; bm}


  let eval_tree ~addr ~source_path (tree : Syn.tree) : Sem.tree * Sem.tree list =
    let fm = {(Sem.empty_frontmatter ~addr) with source_path} in
    Fm.run ~init:fm @@ fun () ->
    Scope.run ~init:addr @@ fun () ->
    Emitted_trees.run ~init:[] @@ fun () ->
    Heap.run ~init:Env.empty @@ fun () ->
    Lex_env.run ~env:Env.empty @@ fun () ->
    Dyn_env.run ~env:Env.empty @@ fun () ->
    let tree = eval_tree_inner ~addr tree in
    let emitted = Emitted_trees.get () in
    tree, emitted

  let run_query = Query_engine.run_query
end
