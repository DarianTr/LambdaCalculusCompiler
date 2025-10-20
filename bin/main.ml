let () =
  let input = "\\x. x" in 
  let tokens = Lexer.tokenize input in 
  List.iter (fun t -> 
    match t with
    | Lexer.LAMBDA -> print_endline "Lambda"
    | Lexer.RPAREN -> print_endline ")"
    | Lexer.LPAREN -> print_endline "("
    | Lexer.DOT-> print_endline "."
    | Lexer.VARIABLE name -> print_endline ("Variable: " ^ name)
  ) tokens;

