open AstLib.Ast
open IR

module R = struct
  open Base.Result

  type 'a t = int -> int * ('a, string) Result.t

  let fail error state = state, fail error
  let return value last = last, return value

  let ( >>= ) (monad : 'a t) f state =
    let last, result = monad state in
    match result with
    | Error e -> last, Error e
    | Ok value -> f value last
  ;;

  let ( let* ) = ( >>= )
  let ( >>| ) m f = m >>= fun x -> return @@ f x
  let fresh last = last + 1, Ok last
  let run m = snd (m 0)

  let map f xs =
    let* res =
      List.fold_left
        (fun acc x ->
          let* acc = acc in
          let* res = f x in
          return (res :: acc))
        (return [])
        xs
    in
    return (List.rev res)
  ;;
end

module RuntimeEnv = struct
  open R

  type runtime_member =
    { name : string
    ; typ : typ
    }

  let apply f x = return @@ eapp (eid (ident_of_definable (ident_letters f.name))) x

  (** add it to infer *)
  let generic = tvar "a"

  let get_head = { name = "GET_HEAD"; typ = tarrow (tlist generic) generic }
  let get_tl = { name = "GET_TALE"; typ = tarrow (tlist generic) (tlist generic) }
  let get_nth = { name = "GET_NTH"; typ = tarrow (ttuple tint generic []) (tvar "b") }
  let not_exhaustive_pm = { name = "RTE_ERROR_MATCH_FAILURE"; typ = tarrow tunit generic }
  let init_env = [ get_head; get_tl; get_nth; not_exhaustive_pm ]
end

module Env = struct
  open R

  let extend env id e = Base.Map.update env id ~f:(fun _ -> e)
  let empty = Base.Map.empty (module Base.String)
  let lookup_env id map = Base.Map.find map id

  let merge m1 m2 =
    Base.Map.fold_right m2 ~init:(return m1) ~f:(fun ~key ~data acc ->
      let* acc = acc in
      match lookup_env key acc with
      | Some _ -> fail "Key intersection"
      | None -> return @@ extend acc key data)
  ;;
end

module RuntimeUtils = struct
  open RuntimeEnv
  open IR

  let apply_get_head head = apply get_head head
  let apply_get_tl tl = apply get_tl tl
  let apply_get_nth n e = apply get_nth (etuple (econst (CInt n)) e [])
  let apply_not_exhaustive_pm () = apply not_exhaustive_pm (econst CUnit)
  let create_var_for_eval n = "EVALUATED_" ^ n

  let create_pop_and_expr_for_eval evaluated =
    let create_pop_for_eval evaluated = pop_pat (pid evaluated) in
    let create_expr_for_eval evaluated =
      eid (ident_of_definable (ident_letters evaluated))
    in
    create_pop_for_eval evaluated, create_expr_for_eval evaluated
  ;;
end
