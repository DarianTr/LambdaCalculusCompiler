type variable = 
  | Var of string
  | VariableErr

type lambda_expression = 
  | Variable of variable 
  | Abstraction of {var: variable; expr: lambda_expression}
  | Application of {left: lambda_expression; right: lambda_expression}
  | ParseErrorExpr
  | EOF

let string_of_variable = function
  | Var name -> name
  | VariableErr -> "<variable_error>"

let rec string_of_lambda = function
  | Variable v ->
      string_of_variable v
  | Abstraction { var; expr } ->
      Printf.sprintf "(λ%s. %s)"
        (string_of_variable var)
        (string_of_lambda expr)
  | Application { left; right } ->
      Printf.sprintf "(%s %s)"
        (string_of_lambda left)
        (string_of_lambda right)
  | ParseErrorExpr ->
      "<parse_error>"
  | EOF ->
      "<eof>"





class parser lexer =  
  object (self) 
    val mutable lex = lexer
  method parse_variable = 
    let tok = lex#next_token in 
     match tok with
      | Lexer.VARIABLE n -> Var(n)
      | _ -> print_endline "variable err"; VariableErr

  method parse_abstraction = 
    let var = self#parse_variable in 
    match var with
    | VariableErr -> ParseErrorExpr
    | Var _ ->  let tok2 = lex#next_token in 
                   match tok2 with
                   | Lexer.DOT -> (
                    let expr = self#parse_expr in 
                     match expr with
                     | ParseErrorExpr -> ParseErrorExpr
                     | _ -> Abstraction({var; expr})
                   )
                  | _ -> print_endline "expected a . after lamba"; ParseErrorExpr
  method parse_expr = 
    let token = lex#next_token in 
    match token with
      | Lexer.VARIABLE name -> Variable(Var(name)) 
      | Lexer.LPAREN -> (let left = self#parse_expr in 
                        let right = self#parse_expr in
                        let peek = lex#next_token in
                        match peek with
                          | Lexer.RPAREN -> let _ = lex#next_token in Application({left; right}) 
                          | _ -> print_endline "expected )"; ParseErrorExpr
                      )
      | Lexer.LAMBDA -> self#parse_abstraction 
      | Lexer.EOF -> EOF
      | _ -> print_endline "Token is not a beginning of a lexpr"; ParseErrorExpr
 end
