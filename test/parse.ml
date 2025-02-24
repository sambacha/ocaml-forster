open Forester_core
open Forester_frontend.Parse

let emit _ = () (* ignore *)

let fatal _ = exit 1

let _ =
  Reporter.run ~emit ~fatal @@ fun () ->
  let good = Result.get_ok @@ parse_string {|
    \title{Good}
    \taxon{Test}
    \author{Testy}

    \p{
      This should parse correctly.
    }
    |}
  in
  Format.printf "parse_good_result:\n%s\n\n" (Code.show good)

let _ =
  Reporter.run ~emit ~fatal @@ fun () ->
  (* Incomplete XML literal should error *)
  let bad, errors = Result.get_error @@ parse_string {|
    \p{Keep me}
    \<foo>[bar]{
      Incomplete fragment, needs another {} at the end.
    }
    |}
  in
  errors |> List.iter (fun (e : Forester_core.Reporter.Message.t Asai.Diagnostic.t) -> Format.printf "error: %s\n" (Asai.Diagnostic.string_of_text e.explanation.value));
  Format.printf "parse_bad_result:\n%s\n\n" (Code.show bad)
