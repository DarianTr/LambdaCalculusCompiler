(* let rec print lexer =  *)
(*   let token = lexer#next_token in  *)
(*   match token with *)
(*   | Frontend.Lexer.LAMBDA -> print_endline "Lambda"; print lexer *)
(*   | Frontend.Lexer.RPAREN -> print_endline ")";  (print lexer) *)
(*   | Frontend.Lexer.LPAREN -> print_endline "(";(print lexer) *)
(*   | Frontend.Lexer.DOT-> print_endline "."; (print lexer) *)
(*   | Frontend.Lexer.VARIABLE name ->  print_endline ("Variable: " ^ name); (print lexer) *)
(*   | Frontend.Lexer.EOF ->  print_endline "End of file"; "" *)
(**)

let () =
  let input = "(\\x . \\y . \\z . ((x z) (y z)) \\a . (a a))" in 
  let lexer = new Frontend.Lexer.lexer input (String.length input) in 
  let parser = new Frontend.Parser.parser lexer in 
  let expr = parser#parse_expr in 
  let runtime = new Runtime.runtime expr in 
  let new_expr = runtime#apply_alpha_conversion expr in
  print_endline (Frontend.Parser.string_of_lambda new_expr);
  (* print_endline (Frontend.Parser.show_lambda_expression new_expr); *)
