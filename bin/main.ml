let rec print lexer = 
  let token = lexer#next_token in 
  match token with
  | Lexer.LAMBDA -> print_endline "Lambda"; print lexer
  | Lexer.RPAREN -> print_endline ")";  (print lexer)
  | Lexer.LPAREN -> print_endline "(";(print lexer)
  | Lexer.DOT-> print_endline "."; (print lexer)
  | Lexer.VARIABLE name ->  print_endline ("Variable: " ^ name); (print lexer)
  | Lexer.EOF ->  print_endline "End of file"; ""


let () =
  let input = "\\x. x" in 
  let lexer = new Lexer.lexer input (String.length input) in 
  print_endline (print lexer)
