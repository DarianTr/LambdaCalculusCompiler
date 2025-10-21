let rec print lexer = 
  let token = lexer#next_token in 
  match token with
  | Frontend.Lexer.LAMBDA -> print_endline "Lambda"; print lexer
  | Frontend.Lexer.RPAREN -> print_endline ")";  (print lexer)
  | Frontend.Lexer.LPAREN -> print_endline "(";(print lexer)
  | Frontend.Lexer.DOT-> print_endline "."; (print lexer)
  | Frontend.Lexer.VARIABLE name ->  print_endline ("Variable: " ^ name); (print lexer)
  | Frontend.Lexer.EOF ->  print_endline "End of file"; ""


let () =
  let input = "\\x. x" in 
  let lexer = new Frontend.Lexer.lexer input (String.length input) in 
  print_endline (print lexer)
