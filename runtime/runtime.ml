module StringSet = Set.Make(String)


  class runtime expr  = 
    object
      val mutable expr: Frontend.Parser.lambda_expression = expr
      val mutable list_of_variables = [] |> StringSet.of_list
    method apply_alpha_conversion cur_expr = 
       match cur_expr with
       | -> 
  end
      



