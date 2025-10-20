

type token =
  | LAMBDA (* '\' is used as a lambda symbol *) 
  | VARIABLE of string
  | LPAREN 
  | RPAREN
  | DOT

let is_variable_char c = 
  match c with
| 'a'..'z' | 'A'..'Z'-> true
| _ -> false

let tokenize input = 
  let len = String.length input in 
  let rec aux i = 
    if i >= len then []
    else
      match input.[i] with
      | '\\' -> LAMBDA :: aux (i+1)  
      | '(' -> LPAREN :: aux (i+1) 
      | ')' -> RPAREN :: aux (i+1) 
      | '.' -> DOT :: aux (i+1)
      | c when is_variable_char c -> 
        let rec aux2 j = 
          if i + j > len then aux(i+j) 
          else if i + j = len then  VARIABLE (String.sub input i j) :: aux (i + j) 
          else 
            match input.[i + j] with
            | c when is_variable_char c -> aux2 (j+1)
            | _ -> VARIABLE (String.sub input i j) :: aux (i + j)  
        in 
        aux2 0
      | _ -> aux (i+1) (*skip unknown chars*)
  in 
  aux 0
