open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option *)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
let rec parse_expr toks =
    match (lookahead toks) with
      | Some Tok_Let -> parse_let_expr toks
      | Some Tok_Fun -> parse_fun_expr toks
      | Some Tok_If -> parse_if_expr toks
      | _ -> parse_or_expr toks
    and parse_let_expr toks = 
        let first = (match_token toks Tok_Let) in
        match (lookahead first) with
            | Some Tok_Rec ->
                let second = (match_token first Tok_Rec) in
                let third = (parse_id_expr second) in 
                let fourth = (match_token second (Tok_ID (third))) in
                let fifth = (match_token fourth Tok_Equal) in
                let (sixth, seventh) = (parse_expr fifth) in
                let eigth = (match_token sixth Tok_In) in
                let (nineth, tenth) = (parse_expr eigth) in
                (nineth, Let (third, true, seventh, tenth))
            | _ ->
                let second = (parse_id_expr first) in 
                let third = (match_token first (Tok_ID (second))) in
                let fourth = (match_token third Tok_Equal) in
                let (fifth, sixth) = (parse_expr fourth) in
                let seventh = (match_token fifth Tok_In) in
                let (eigth, nineth) = (parse_expr seventh) in
                (eigth, Let (second, false, sixth, nineth))
    and parse_fun_expr toks =
        let first = (match_token toks Tok_Fun) in
        let second = (parse_id_expr first) in
        let third = (match_token first (Tok_ID (second))) in 
        let fourth = (match_token third Tok_Arrow) in 
        let (fifth, sixth) = (parse_expr fourth) in
        (fifth, Fun (second, sixth))
    and parse_if_expr toks =
        let first = (match_token toks Tok_If) in
        let (second, third) = (parse_expr first) in
        let fourth = (match_token second Tok_Then) in
        let (fifth, sixth) = (parse_expr fourth) in
        let seventh = (match_token fifth Tok_Else) in
        let (eigth, nineth) = (parse_expr seventh) in
        (eigth, If (third, sixth, nineth))
    and parse_funcall_expr toks = 
        let (first, second) = (parse_primary_types_expr toks) in
        match (lookahead first) with
            | Some (Tok_ID x) -> 
                let (third, fourth) = (parse_primary_types_expr first) in
                (third, FunctionCall (second, fourth)) 
            | Some (Tok_Bool x) -> 
                let (third, fourth) = (parse_primary_types_expr first) in
                (third, FunctionCall (second, fourth)) 
            | Some (Tok_Int x) -> 
                let (third, fourth) = (parse_primary_types_expr first) in
                (third, FunctionCall (second, fourth)) 
            | Some (Tok_String x) -> 
                let (third, fourth) = (parse_primary_types_expr first) in
                (third, FunctionCall (second, fourth)) 
            | Some Tok_LParen ->
                let (third, fourth) = (parse_primary_types_expr first) in
                (third, FunctionCall (second, fourth)) 
            | _ -> (first, second)
    and parse_primary_types_expr toks = 
        match (lookahead toks) with
            | Some (Tok_ID x) -> 
                let first = (parse_id_expr toks) in
                let second = (match_token toks (Tok_ID x)) in
                (second, ID (first))
            | Some (Tok_Bool x) -> 
                let first = (parse_bool_expr toks) in
                let second = (match_token toks (Tok_Bool x)) in
                (second, Value (Bool (first)))
            | Some (Tok_Int x) -> 
                let first = (parse_int_expr toks) in
                let second = (match_token toks (Tok_Int x)) in
                (second, Value (Int (first)))
            | Some (Tok_String x) -> 
                let first = (parse_string_expr toks) in
                let second = (match_token toks (Tok_String x)) in
                (second, Value (String (first)))
            | Some Tok_LParen -> 
                let first = (match_token toks Tok_LParen) in
                let (second, third) = (parse_expr first) in
                let fourth = (match_token second Tok_RParen) in
                (fourth, third)
            | _ -> raise (InvalidInputException "Error: Parse Expression")
    and parse_id_expr toks =
        match (lookahead toks) with
            | Some (Tok_ID x) -> x
            | _ -> raise (InvalidInputException "Error: Parse Expression")
    and parse_bool_expr toks = 
        match (lookahead toks) with
            | Some (Tok_Bool x) -> x
            | _ -> raise (InvalidInputException "Error: Parse Expression")
    and parse_int_expr toks = 
        match (lookahead toks) with
            | Some (Tok_Int x) -> x
            | _ -> raise (InvalidInputException "Error: Parse Expression")
    and parse_string_expr toks = 
        match (lookahead toks) with
            | Some (Tok_String x) -> x
            | _ -> raise (InvalidInputException "Error: Parse Expression")
    and parse_or_expr toks =
        let (first, second) = (parse_and_expr toks) in
        match (lookahead first) with
            | Some Tok_Or -> 
                let third = (match_token first Tok_Or) in
                let (fourth, fifth) = (parse_and_expr third) in 
                (fourth, Binop (Or, second, fifth))
            | _ -> (first, second)
    and parse_and_expr toks = 
        let (first, second) = (parse_equal_expr toks) in
        match (lookahead first) with
            | Some Tok_And -> 
                let third = (match_token first Tok_And) in
                let (fourth, fifth) = (parse_and_expr third) in 
                (fourth, Binop (And, second, fifth))
            | _ -> (first, second)
    and parse_equal_expr toks =
        let (first, second) = (parse_less_or_greater_expr) toks in
        match (lookahead first) with
            | Some Tok_Equal ->
                let third = (match_token first Tok_Equal) in
                let (fourth, fifth) = (parse_equal_expr third) in 
                (fourth, Binop (Equal, second, fifth))
            | Some Tok_NotEqual ->
                let third = (match_token first Tok_NotEqual) in
                let (fourth, fifth) = (parse_equal_expr third) in 
                (fourth, Binop (NotEqual, second, fifth))
            | _ -> (first, second)
    and parse_less_or_greater_expr toks =
        let (first, second) = (parse_add_expr toks) in
        match (lookahead first) with
            | Some Tok_Less ->
                let third = (match_token first Tok_Less) in
                let (fourth, fifth) = (parse_less_or_greater_expr third) in 
                (fourth, Binop (Less, second, fifth))
            | Some Tok_LessEqual ->
                let third = (match_token first Tok_LessEqual) in
                let (fourth, fifth) = (parse_less_or_greater_expr third) in 
                (fourth, Binop (LessEqual, second, fifth))
            | Some Tok_Greater ->
                let third = (match_token first Tok_Greater) in
                let (fourth, fifth) = (parse_less_or_greater_expr third) in 
                (fourth, Binop (Greater, second, fifth))
            | Some Tok_GreaterEqual ->
                let third = (match_token first Tok_GreaterEqual) in
                let (fourth, fifth) = (parse_less_or_greater_expr third) in 
                (fourth, Binop (GreaterEqual, second, fifth))
            | _ -> (first, second)
    and parse_add_expr toks = 
        let (first, second) = (parse_mult_expr toks) in
        match (lookahead first) with
            | Some Tok_Add -> 
                let third = (match_token first Tok_Add) in
                let (fourth, fifth) = (parse_add_expr third) in 
                (fourth, Binop (Add, second, fifth))
            | Some Tok_Sub -> 
                let third = (match_token first Tok_Sub) in
                let (fourth, fifth) = (parse_add_expr third) in 
                (fourth, Binop (Sub, second, fifth))
            | _ -> (first, second)
    and parse_mult_expr toks = 
        let (first, second) = (parse_concat_expr toks) in
        match (lookahead first) with
            | Some Tok_Mult -> 
                let third = (match_token first Tok_Mult) in
                let (fourth, fifth) = (parse_mult_expr third) in 
                (fourth, Binop (Mult, second, fifth))
            | Some Tok_Div -> 
                let third = (match_token first Tok_Div) in
                let (fourth, fifth) = (parse_mult_expr third) in 
                (fourth, Binop (Div, second, fifth))
            | _ -> (first, second)
    and parse_concat_expr toks =
        let (first, second) = (parse_unary_expr toks) in
        match (lookahead first) with
            | Some Tok_Concat -> 
              let third = (match_token first Tok_Concat) in
              let (fourth, fifth) = (parse_concat_expr third) in 
              (fourth, Binop (Concat, second, fifth))
            | _ -> (first, second)
    and parse_unary_expr toks =
        match (lookahead toks) with
            | Some Tok_Not ->
                let first = (match_token toks Tok_Not) in
                let (second, third) = (parse_unary_expr first) in 
                (second, Not (third))
            | _ -> (parse_funcall_expr toks)
  
(* Part 3: Parsing mutop *)
let rec parse_mutop toks =
    match (lookahead toks) with
        | Some Tok_Def -> 
            (parse_def_expr toks)
        | Some Tok_DoubleSemi -> 
            ((match_token toks Tok_DoubleSemi), NoOp)
        | _ ->
            let (first, second) = (parse_expr toks) in
            let third = (match_token first Tok_DoubleSemi) in
            (third, Expr (second))
    and parse_def_expr toks =
        let first = (match_token toks Tok_Def) in
        let second = (parse_id_expr first) in
        let third = (match_token first (Tok_ID (second))) in
        let fourth = (match_token third Tok_Equal) in
        let (fifth, sixth) = parse_expr (fourth) in
        let seventh = (match_token fifth Tok_DoubleSemi) in
        (seventh, Def (second, sixth))