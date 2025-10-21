
type token =
  | LAMBDA (* '\' is used as a lambda symbol *) 
  | VARIABLE of string
  | LPAREN 
  | RPAREN
  | DOT
  | EOF

let is_variable_char c = 
  match c with
| 'a'..'z' | 'A'..'Z'-> true
| _ -> false



class lexer input len =  
  object (self)
    val mutable current_position: int = 0     
    val mutable input: string = input 
    val mutable len: int = len 
    method peek = 
        if  current_position >= String.length input then EOF 
        else 
          match input.[current_position] with
           | '\\' -> LAMBDA 
           | '(' -> LPAREN  
           | ')' -> RPAREN
           | '.' -> DOT 
           | c when is_variable_char c -> 
            let rec aux2 j = 
            if current_position + j = len then  VARIABLE (String.sub input current_position j) 
            else 
              match input.[current_position + j] with
              | c when is_variable_char c -> aux2 (j+1)
              | _ -> VARIABLE (String.sub input current_position j)  
            in 
            aux2 0
          | _ -> 
            current_position <- current_position + 1;
            self#peek
  method next_token = 
    let token = self#peek in 
    match token with 
      | VARIABLE name -> current_position <- current_position + (String.length name); token
      | EOF -> token
      | _ -> current_position <- current_position + 1; token
end
