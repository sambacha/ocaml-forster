
module Make (S : sig type t end) () =
struct
  type 'a Effect.t +=
    | Observe : (S.t -> unit) -> unit Effect.t

  let observe f = Effect.perform @@ Observe f

  let run f =
    let open Effect.Deep in
    let updaters = Queue.create () in
    updaters, try_with f ()
      { effc =
          fun (type a) (eff : a Effect.t) ->
            match eff with
            | Observe f -> Option.some @@ fun (k : (a, _) continuation) ->
              continue k @@ Queue.add f updaters
            | _ -> None }

end