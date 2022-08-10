open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)      
let tokenize input = 
    let rec tokenize_helper pos str =
        if pos >= String.length str then []
        else if Str.string_match (Str.regexp " ") str pos then
            tokenize_helper (pos + 1) str
        else if (Str.string_match (Str.regexp "true") str pos) then
            let token = Str.matched_string str in
                (Tok_Bool true)::(tokenize_helper (pos + String.length token) str)
        else if (Str.string_match (Str.regexp "false") str pos) then
            let token = Str.matched_string str in
                (Tok_Bool false)::(tokenize_helper (pos + String.length token) str)
        else if (Str.string_match (Str.regexp "(-[0-9]+)") str pos) then
          let token = Str.matched_string str in
              let refined_token = String.sub token 1 ((String.length token) - 2) in
                  (Tok_Int (int_of_string refined_token))::(tokenize_helper (pos + String.length token) str)
        else if (Str.string_match (Str.regexp "[0-9]+") str pos) then
              let token = Str.matched_string str in
                  (Tok_Int (int_of_string token))::(tokenize_helper (pos + String.length token) str)
        else if (Str.string_match (Str.regexp "\"[^\"]*\"") str pos) then
            let token = Str.matched_string str in
                let refined_token = String.sub token 1 ((String.length token) - 2) in
                    (Tok_String (refined_token))::(tokenize_helper (pos + String.length token) str)
        else if (Str.string_match (Str.regexp "if ") str pos) then
            Tok_If::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp "then ") str pos then
            Tok_Then::(tokenize_helper (pos + 4) str)
        else if Str.string_match (Str.regexp "else ") str pos then
            Tok_Else::(tokenize_helper (pos + 4) str)
        else if Str.string_match (Str.regexp "let ") str pos then
            Tok_Let::(tokenize_helper (pos + 3) str)
        else if Str.string_match (Str.regexp "def ") str pos then
            Tok_Def::(tokenize_helper (pos + 3) str)
        else if Str.string_match (Str.regexp "in ") str pos then
            Tok_In::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp "rec ") str pos then
            Tok_Rec::(tokenize_helper (pos + 3) str)
        else if Str.string_match (Str.regexp "fun ") str pos then
            Tok_Fun::(tokenize_helper (pos + 3) str)
        else if Str.string_match (Str.regexp "not ") str pos then
            Tok_Not::(tokenize_helper (pos + 3) str)
        else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") str pos) then
            let token = Str.matched_string str in
                (Tok_ID token)::(tokenize_helper (pos + String.length token) str)
        else if Str.string_match (Str.regexp "->") str pos then
            Tok_Arrow::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp ">=") str pos then
            Tok_GreaterEqual::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp "<=") str pos then
            Tok_LessEqual::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp "<>") str pos then
            Tok_NotEqual::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp "||") str pos then
            Tok_Or::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp "&&") str pos then
            Tok_And::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp ";;") str pos then
            Tok_DoubleSemi::(tokenize_helper (pos + 2) str)
        else if Str.string_match (Str.regexp "\\+") str pos then
            Tok_Add::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp "\\^") str pos then
            Tok_Concat::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp "\\*") str pos then
            Tok_Mult::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp "-") str pos then
            Tok_Sub::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp "/") str pos then
            Tok_Div::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp "(") str pos then
            Tok_LParen::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp ")") str pos then
            Tok_RParen::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp "=") str pos then
            Tok_Equal::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp "<") str pos then
            Tok_Less::(tokenize_helper (pos + 1) str)
        else if Str.string_match (Str.regexp ">") str pos then
            Tok_Greater::(tokenize_helper (pos + 1) str)
        else raise (InvalidInputException "Error: Tokenize")
    in tokenize_helper 0 input
   