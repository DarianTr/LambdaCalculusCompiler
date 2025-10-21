module StringSet = Set.Make(String)


let rec num_to_letters n = 
  let letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let c = letters.[n mod 52] in
  let new_string = String.make 1 c  in
  if n <= 51 then new_string  
  else 
    new_string ^  (num_to_letters (n / 52))
  
    


class runtime expr  = 
    object(self)
      val mutable expr: Frontend.Parser.lambda_expression = expr
      val mutable list_of_variables = [] |> StringSet.of_list
    method replace_var var1 name2 replacement = 
      match var1 with Frontend.Parser.Var name -> if name = name2 then Frontend.Parser.Variable(Frontend.Parser.Var(replacement)) else Frontend.Parser.Variable(var1) | _ -> expr
    method replace_all var replacement expr = 
      match expr with
        | Frontend.Parser.Variable name -> self#replace_var name var replacement
        | Frontend.Parser.Abstraction { var = Frontend.Parser.Var name; expr = body} -> if name = var then expr (*WATCH OUT SHADOWING, STOP APPLYING ALPHA ON THAT BRANCH*) 
                                                                                else let new_expr = self#replace_all var replacement body in  Frontend.Parser.Abstraction{var = Frontend.Parser.Var(name); expr = new_expr}
        | Frontend.Parser.Application { left = left; right = right} -> let l = self#replace_all var replacement left in let r = self#replace_all var replacement right in Frontend.Parser.Application{left = l; right = r}
        | _ -> expr 
    method apply_alpha_conversion cur_expr = 
       match cur_expr with
      | Frontend.Parser.Abstraction { var = Frontend.Parser.Var name; expr = body } -> 
        let new_name = num_to_letters (StringSet.cardinal list_of_variables)   in
        let new_expr = (self#replace_all name new_name body) in 
        let new_var = Frontend.Parser.Var(new_name) in
        list_of_variables <- StringSet.add new_name list_of_variables;
        print_endline new_name;
        Frontend.Parser.Abstraction {var = new_var; expr = new_expr}
| Frontend.Parser.Application {left; right} -> let l = self#apply_alpha_conversion left in let r = self#apply_alpha_conversion right in Frontend.Parser.Application{left = l; right = r}
      | _ -> print_endline "no match"; cur_expr 
  end
      



