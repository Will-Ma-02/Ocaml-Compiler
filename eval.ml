open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
    match e with
      | Value x -> 
          x
      | ID x ->
          lookup env x
      | Fun (x, y) ->
          Closure (env, x, y)
      | Not x -> 
          let result = (eval_expr env x) in
          (match result with
              | Bool b -> Bool (not (b))
              | _ -> raise (TypeError "Error: Not"))
      | Binop (x, y, z) ->
          if x = Add then
              let result1 = (eval_expr env y) in
              let result2 = (eval_expr env z) in
              (match result1, result2 with
                  | Int i, Int j -> Int (i + j)
                  | _, _ -> raise (TypeError "Error: Add"))
          else if x = Sub then
              let result1 = (eval_expr env y) in
              let result2 = (eval_expr env z) in
              (match result1, result2 with
                  | Int i, Int j -> Int (i - j)
                  | _, _ -> raise (TypeError "Error: Sub"))
          else if x = Mult then
              let result1 = (eval_expr env y) in
              let result2 = (eval_expr env z) in
              (match result1, result2 with
                    | Int i, Int j -> Int (i * j)
                    | _, _ -> raise (TypeError "Error: Mult"))
          else if x = Div then
              let result1 = (eval_expr env y) in
              let result2 = (eval_expr env z) in
              (match result1, result2 with
                  | Int i, Int 0 -> raise (DivByZeroError)
                  | Int i, Int j -> Int (i / j)
                  | _, _ -> raise (TypeError "Error: Div"))
          else if (x = Less || x = Greater || x = LessEqual || x = GreaterEqual) then
              let result1 = (eval_expr env y) in
              let result2 = (eval_expr env z) in
              (match result1, result2 with
                  | Int i, Int j ->
                      if (x = Less && i < j) then 
                          Bool (true)
                      else if (x = Greater && i > j) then 
                          Bool (true)
                      else if (x = LessEqual && i <= j) then 
                          Bool (true)
                      else if (x = Greater && i >= j) then 
                          Bool (true)
                      else 
                          Bool (false)
                  | _, _ -> raise (TypeError "Error: Relations"))
            else if x = Concat then
                let result1 = (eval_expr env y) in
                let result2 = (eval_expr env z) in
                (match result1, result2 with
                    | String s, String t -> String (s ^ t)
                    | _, _ -> raise (TypeError "Error: Concat"))
            else if (x = Equal || x = NotEqual) then
                let result1 = (eval_expr env y) in
                let result2 = (eval_expr env z) in
                (match result1, result2 with
                    | Int i, Int j -> 
                        if (x = Equal && i = j) then
                            Bool (true)
                        else if (x = NotEqual && i <> j) then
                            Bool (true)
                        else
                            Bool (false)
                    | String s, String t -> 
                        if (x = Equal && s = t) then
                            Bool (true)
                        else if (x = NotEqual && s <> t) then
                            Bool (true)
                        else
                            Bool (false)
                    | Bool b, Bool c -> 
                        if (x = Equal && b = c) then
                            Bool (true)
                        else if (x = NotEqual && b <> c) then
                            Bool (true)
                        else
                            Bool (false)
                    | _, _ -> raise (TypeError "Error: Equality"))
              else if x = Or then
                  let result1 = (eval_expr env y) in
                  let result2 = (eval_expr env z) in 
                  (match result1, result2 with
                      | Bool b, Bool c -> 
                          Bool (b || c)
                      | _, _ -> raise (TypeError "Error: Or"))
              else if x = And then
                  let result1 = (eval_expr env y) in
                  let result2 = (eval_expr env z) in 
                  (match result1, result2 with
                      | Bool b, Bool c -> 
                          Bool (b && c)
                      | _, _ -> raise (TypeError "Error: And"))
              else
                  raise (TypeError "Error: Binop")
      | If (x, y, z) ->
            let result = (eval_expr env x) in
            (match result with
                | Bool b -> 
                    if b = true then 
                        (eval_expr env y)
                    else 
                        (eval_expr env z)
                | _ -> raise (TypeError "Error: If"))
      | FunctionCall (x, y) -> 
          let result1 = (eval_expr env x) in
          let result2 = (eval_expr env y) in
          (match result1 with
              | Closure (c, d, e) ->
                  (eval_expr (extend c d result2) e)
              | _ -> raise (TypeError "Error: Function Call"))
      | Let (x, y, z, w) ->
          if y = true then
              let result1 = (eval_expr (extend_tmp env x) z) in 
              (update (extend_tmp env x) x result1) ; (eval_expr (extend_tmp env x) w)
          else
              (eval_expr (extend env x (eval_expr env z)) w)

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
    match m with
        | Def (x, y) ->
            let result1 = (extend_tmp env x) in
            let result2 = (eval_expr result1 y) in
            (update result1 x result2) ; (result1, Some result2)
        | Expr x -> 
            (env, Some (eval_expr env x))
        | NoOp -> 
            (env, None)