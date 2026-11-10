type const = Int of int | Double of float | Bool of bool | Error | String of string | Name of string | Unit


type com = Push of const | Pop | Add | Sub | Mul | Div | Rem | Neg | Swap | ToString | Println | Quit | Cat | And
| Or | Not | Equal | LessThan | Bind | If | Let | End


type prog = com list


let interpreter ((inFile : string) , (outFile : string)) : unit =


  (* This chunk of code from lines 14-21 is copy/pasted from the pangram homework.
      This chunk reads the input file line by line and builds a list of strings and sets
      up the output channel for writing the interpreter output to "output.txt" *)
  let ic = open_in inFile in
  let oc = open_out outFile in
  let rec loop_read acc =
    try
      let l = input_line ic in loop_read (l::acc)
    with
    | End_of_file -> List.rev acc in
  let ls_str = loop_read [] in


  (* Helper function for name_check that returns true if the character is a letter
      and false otherwise *)
  let is_letter (c : char) : bool =
    match c with
    | 'a' .. 'z' -> true
    | 'A' .. 'Z' -> true
    | _ -> false in


  (* Helper function for checking if a string is a valid name. The project assumes correct inputs
      so all I check is that it either starts with a letter or an underscore. *)
  let name_check (s : string) : bool =
    if String.get s 0 = '_' then
      true
    else if is_letter (String.get s 0) then
      true
    else
      false in
   
 
  (* push_const matches s with a const value. If s is not a valid const value, it returns Error.
      This function's sole purpose is to convert a string to a const value *)
  let push_const (s : string) : const =
    match s with
    | ":true:" -> Bool true
    | ":false:" -> Bool false
    | ":error:" -> Error
    | ":unit:" -> Unit
    | _ ->
      match int_of_string_opt s with
      | Some i -> Int i
      | None ->
        match float_of_string_opt s with
        | Some d -> Double d
        | None ->
          if String.length s >= 2 && String.get s 0 = '"' && String.get s (String.length s - 1) = '"' then
            String (String.sub s 1 (String.length s - 2))
          else if name_check s then
            Name s
          else
            Error in
     
  (* push just pushes a const onto the stack. The actual hard work of pushing correct values is done
      through to_command and push_const. *)
  let push (stack : const list) (p : const) : const list =
    p::stack in
 
  (* pop removes the top value from the stack. If the stack is empty, an error literal will be
      pushed onto the stack. *)
  let pop (stack : const list) : const list =
    match stack with
    | [] -> push stack Error
    | hd::tl -> tl in


  (* bind_list search is a helper function that takes a name and the bind list. We compare the name binding 
      of the head of the bind list with the parameter name. If they are equal, return the associated value.
      Else continue searching the bind list for a matching name. If no name is found, then return an Error. *)
  let rec bind_list_search (name : string) (bind_list : (string*const) list) (parent_scope_list : (const list * (string*const) list) list): const =
    match bind_list with
    | (bind_name, bind_val)::tl ->
      if name = bind_name then
        bind_val
      else 
        bind_list_search name tl parent_scope_list
    (* If the current scope's bind_list has no matching name, we check the bind_list of the parent scope. we 
        repeat until we find a matching name or exhaust all scopes and return an Error *)
    | [] -> (
      match parent_scope_list with
      | (parent_stack, parent_bind)::tl -> bind_list_search name parent_bind tl
      | [] -> Error
    ) in


  (* add refers to integer OR floating point addition. Since this is a binary operator, it consumes
      the top two values in the stack, calculates the sum, and pushes the result back onto the stack. Only
      works with a combination of 2 integer and floating point values *)
  let add (stack : const list) bind_list parent_scope_list: const list =
    match stack with
    | (Int y)::(Int x)::tl -> (Int (x + y))::tl
    | (Double y)::(Double x)::tl -> (Double (x +. y))::tl
    | (Int y)::(Double x)::tl -> (Double (x +. float_of_int y))::tl
    | (Double y)::(Int x)::tl -> (Double (float_of_int x +. y))::tl
    (* Adding binding handling. Essentially adding addition of variables *)
    | (Int y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Int (x + y))::tl
      | Double x -> (Double (x +. float_of_int y))::tl
      | _ -> push stack Error)
    | (Name n)::(Int x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int y -> (Int (x + y))::tl
      | Double y -> (Double (float_of_int x +. y))::tl
      | _ -> push stack Error)
    | (Double y)::(Name n)::tl ->(
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Double (float_of_int x +. y))::tl
      | Double x -> (Double (x +. y))::tl
      | _ -> push stack Error)
    | (Name n)::(Double x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int y -> (Double (x +. float_of_int y))::tl
      | Double y -> (Double (x +. y))::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      let b = bind_list_search m bind_list parent_scope_list in
      match a,b with
      | Int x, Int y -> (Int (x + y))::tl
      | Double x, Double y -> (Double (x +. y))::tl
      | Int x, Double y -> (Double (float_of_int x +. y))::tl
      | Double x, Int y -> (Double (x +. float_of_int y))::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* sub refers to integer OR floating point subtraction. Works like add*)
  let sub (stack : const list) bind_list parent_scope_list: const list =
    match stack with
    | (Int y)::(Int x)::tl -> (Int (x - y))::tl
    | (Double y)::(Double x)::tl -> (Double (x -. y))::tl
    | (Double y)::(Int x)::tl -> (Double (float_of_int x -. y))::tl
    | (Int y)::(Double x)::tl -> (Double (x -. float_of_int y))::tl
    (* Adding binding handling. Essentially adding subtraction of variables *)
    | (Int y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Int (x - y))::tl
      | Double x -> (Double (x -. float_of_int y))::tl
      | _ -> push stack Error)
    | (Name n)::(Int x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int y -> (Int (x - y))::tl
      | Double y -> (Double (float_of_int x -. y))::tl
      | _ -> push stack Error)
    | (Double y)::(Name n)::tl ->(
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Double (float_of_int x -. y))::tl
      | Double x -> (Double (x -. y))::tl
      | _ -> push stack Error)
    | (Name n)::(Double x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int y -> (Double (x -. float_of_int y))::tl
      | Double y -> (Double (x -. y))::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      let b = bind_list_search m bind_list parent_scope_list in
      match a,b with
      | Int x, Int y -> (Int (x - y))::tl
      | Double x, Double y -> (Double (x -. y))::tl
      | Int x, Double y -> (Double (float_of_int x -. y))::tl
      | Double x, Int y -> (Double (x -. float_of_int y))::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* mul refers to integer OR floating point multiplication. Works like add and sub *)
  let mul (stack : const list) bind_list parent_scope_list: const list =
    match stack with
    | (Int y)::(Int x)::tl -> (Int (x * y))::tl
    | (Double y)::(Double x)::tl -> (Double (x *. y))::tl
    | (Int y)::(Double x)::tl -> (Double (x *. float_of_int y))::tl
    | (Double y)::(Int x)::tl -> (Double (float_of_int x *. y))::tl
    (* Adding binding handling. Essentially adding multiplication of variables *)
    | (Int y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Int (x * y))::tl
      | Double x -> (Double (x *. float_of_int y))::tl
      | _ -> push stack Error)
    | (Name n)::(Int x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int y -> (Int (x * y))::tl
      | Double y -> (Double (float_of_int x *. y))::tl
      | _ -> push stack Error)
    | (Double y)::(Name n)::tl ->(
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Double (float_of_int x *. y))::tl
      | Double x -> (Double (x *. y))::tl
      | _ -> push stack Error)
    | (Name n)::(Double x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int y -> (Double (x *. float_of_int y))::tl
      | Double y -> (Double (x *. y))::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      let b = bind_list_search m bind_list parent_scope_list in
      match a,b with
      | Int x, Int y -> (Int (x * y))::tl
      | Double x, Double y -> (Double (x *. y))::tl
      | Int x, Double y -> (Double (float_of_int x *. y))::tl
      | Double x, Int y -> (Double (x *. float_of_int y))::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* div refers to integer OR floating point division. Works like add, sub, and mul. We
      have to account for division by zero *)
  let div (stack : const list) bind_list parent_scope_list: const list =
    match stack with
    | (Int 0)::tl -> push stack Error
    | (Double 0.0)::tl -> push stack Error
    | (Int y)::(Int x)::tl -> (Int (x / y))::tl
    | (Double y)::(Double x)::tl -> (Double (x /. y))::tl
    | (Int y)::(Double x)::tl -> (Double (x /. float_of_int y))::tl
    | (Double y)::(Int x)::tl -> (Double (float_of_int x /. y))::tl
    (* Adding binding handling. Essentially adding addition of variables *)
    | (Int y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Int (x / y))::tl
      | Double x -> (Double (x /. float_of_int y))::tl
      | _ -> push stack Error)
    | (Name n)::(Int x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int 0 -> push stack Error
      | Double 0.0 -> push stack Error
      | Int y -> (Int (x / y))::tl
      | Double y -> (Double (float_of_int x /. y))::tl
      | _ -> push stack Error)
    | (Double y)::(Name n)::tl ->(
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Double (float_of_int x /. y))::tl
      | Double x -> (Double (x /. y))::tl
      | _ -> push stack Error)
    | (Name n)::(Double x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int 0 -> push stack Error
      | Double 0.0 -> push stack Error
      | Int y -> (Double (x /. float_of_int y))::tl
      | Double y -> (Double (x /. y))::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      let b = bind_list_search m bind_list parent_scope_list in
      match a,b with
      | Int x, Int 0 -> push stack Error
      | Double x, Int 0 -> push stack Error
      | Int x, Double 0.0 -> push stack Error
      | Double x, Double 0.0-> push stack Error
      | Int x, Int y -> (Int (x / y))::tl
      | Double x, Double y -> (Double (x /. y))::tl
      | Int x, Double y -> (Double (float_of_int x /. y))::tl
      | Double x, Int y -> (Double (x /. float_of_int y))::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* rem refers to the remainder of integer division. ONLY applies to integers in this case.
      Works like div. *)
  let rem (stack : const list) bind_list parent_scope_list: const list =
    match stack with
    | (Int 0)::(Int x)::tl -> push stack Error
    | (Int y)::(Int x)::tl -> (Int (x mod y))::tl
    (* Adding binding handling. Essentially adding rem of variables. *)
    | (Int 0)::(Name n)::tl -> push stack Error
    | (Name n)::(Int x)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int 0 -> push stack Error
      | Int y -> (Int (x mod y))::tl
      | _ -> push stack Error)
    | (Int y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      match a with
      | Int x -> (Int (x mod y))::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_scope_list in
      let b = bind_list_search m bind_list parent_scope_list in
      match a,b with
      | _, Int 0 -> push stack Error
      | Int x, Int y -> (Int (x mod y))::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* neg calculates the negation of an integer OR double. Negation of 0 is still 0. *)
  let neg (stack : const list) bind_list parent_scope_list : const list =
    match stack with
    | (Int x)::tl -> (Int (-x))::tl
    | (Double x)::tl -> (Double (-.x))::tl
    (* Adding binding handling for negation *)
    | (Name n)::tl -> (
      let x = bind_list_search n bind_list parent_scope_list in
      match x with
      | Int i -> (Int (-i))::tl
      | Double d -> (Double (-.d))::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* swap interchanges  the top two elements in the stack, meaning the first elements
      becomes the second and the second becomes the first. *)
  let swap (stack : const list) bind_list : const list =
    match stack with
    | x::y::tl -> y::x::tl
    | _ -> push stack Error in


  (* The cat command computes the concatenation of the top two elements in the stack and pushes
    the result onto the stack. The top two values of the stack x and y are popped off and
    the result is the string y concatenated onto x. Pushes an error if there is only one element
    on the stack, the stack is empty, or if either of the elements aren't a String. *)
  let cat (stack : const list) bind_list parent_bind_list : const list =
    match stack with
    | (String y)::(String x)::tl -> String (x ^ y)::tl
    (* Adding binding handling for concatting names *)
    | (String y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | String x -> String (x ^ y)::tl
      | _ -> push stack Error)
    | (Name m)::(String x)::tl -> (
      let a = bind_list_search m bind_list parent_bind_list in
      match a with
      | String y -> String (x ^ y)::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let b = bind_list_search m bind_list parent_bind_list in
      let a = bind_list_search n bind_list parent_bind_list in
      match a,b with
      | String x, String y -> String (x ^ y)::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* The command and performs the logical conjunction of the top two elements in the stack
      and pushes the result (a single value) on to the stack. Will push an error if there is
      only one element on the stack, the stack is empty, either of the top two elements aren't
      booleans. *)
  let and_com (stack : const list) bind_list parent_bind_list: const list =
    match stack with
    | (Bool y)::(Bool x)::tl -> Bool(x && y)::tl
    (* Adding binding handling for logical and for names *)
    | (Bool y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | Bool x -> Bool(x && y)::tl
      | _ -> push stack Error)
    | (Name m)::(Bool x)::tl -> (
      let a = bind_list_search m bind_list parent_bind_list in
      match a with
      | Bool y -> Bool(x && y)::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let b = bind_list_search m bind_list parent_bind_list in
      let a = bind_list_search n bind_list parent_bind_list in
      match a,b with
      | Bool x, Bool y -> Bool(x && y)::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* The command or performs the logical disjunction of the top two elements in the stack and
      pushes the result (a single value) onto the stack. Pushes an erro if there is only one element
      on the stack, the stack is empty, either of the top two elements aren't booleans. *)
  let or_com (stack : const list) bind_list parent_bind_list : const list =
    match stack with
    | (Bool y)::(Bool x)::tl -> Bool(x || y)::tl
    (* Adding binding handling for logical or for names *)
    | (Bool y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | Bool x -> Bool(x || y)::tl
      | _ -> push stack Error)
    | (Name m)::(Bool x)::tl -> (
      let a = bind_list_search m bind_list parent_bind_list in
      match a with
      | Bool y -> Bool(x || y)::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let b = bind_list_search m bind_list parent_bind_list in
      let a = bind_list_search n bind_list parent_bind_list in
      match a,b with
      | Bool x, Bool y -> Bool(x || y)::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* The command not p erforms the logical negation of the top element in the stack and pushes the
      result (a single value) onto the stack. Since the op erator is unary, it only consumes the top
      value from the stack. Pushes an error if the stack is empty or the top element isn't a boolean. *)
  let not_com (stack : const list) bind_list parent_bind_list : const list =
    match stack with
    | (Bool x)::tl -> Bool(not x)::tl
    (* Adding binding handling for logical negation *)
    | (Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | Bool x -> Bool(not x)::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* The command equal refers to numeric equality (so you are not supporting string comparisons). The
      command should handle both Integer as well as floating point values. However, an Integer and a
      Double are not to be treated the same. For example, 7 and 7.0 are NOT comparable. This operator
      consumes the top two values on the stack and pushes the result (a single boolean value) onto the stack.
      Pushes an error if only one element in the stack, stack is empty, if either of the top two elements aren't
      both Integers OR both Doubles. *)
  let equal_com (stack : const list) bind_list parent_bind_list : const list =
    match stack with
    | (Int y)::(Int x)::tl -> Bool(x = y)::tl
    | (Double y)::(Double x)::tl -> Bool(x = y)::tl
    (* Adding binding handling for logical equivalence *)
    | (Int y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | Int x -> Bool(x = y)::tl
      | _ -> push stack Error)
    | (Name m)::(Int x)::tl -> (
      let a = bind_list_search m bind_list parent_bind_list in
      match a with
      | Int y -> Bool(x = y)::tl
      | _ -> push stack Error)
    | (Double y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | Double x -> Bool(x = y)::tl
      | _ -> push stack Error)
    | (Name m)::(Double x)::tl -> (
      let a = bind_list_search m bind_list parent_bind_list in
      match a with
      | Double y -> Bool(x = y)::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let b = bind_list_search m bind_list parent_bind_list in
      let a = bind_list_search n bind_list parent_bind_list in
      match a,b with
      | Int x, Int y -> Bool(x = y)::tl
      | Double x, Double y -> Bool(x = y)::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* The command lessThan refers to numeric less than ordering. This operator consumes the top two
      values on the stack and pushes the result (a single boolean value) onto the stack. The command
      should handle b oth Integer as well as floating point values. However, an Integer and a Double
      are not to be treated the same. For example, 7 and 7.0 are NOT comparable. Push an error if there
      is only one element, the stack is empty, or if the top two elements aren't both Integers OR both
      Doubles. *)
  let lessThan_com (stack : const list) bind_list parent_bind_list: const list =
    match stack with
    | (Int y)::(Int x)::tl -> Bool(x < y)::tl
    | (Double y)::(Double x)::tl -> Bool(x < y)::tl
    (* Adding binding handling for less than ordering *)
    | (Int y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | Int x -> Bool(x < y)::tl
      | _ -> push stack Error)
    | (Name m)::(Int x)::tl -> (
      let a = bind_list_search m bind_list parent_bind_list in
      match a with
      | Int y -> Bool(x < y)::tl
      | _ -> push stack Error)
    | (Double y)::(Name n)::tl -> (
      let a = bind_list_search n bind_list parent_bind_list in
      match a with
      | Double x -> Bool(x < y)::tl
      | _ -> push stack Error)
    | (Name m)::(Double x)::tl -> (
      let a = bind_list_search m bind_list parent_bind_list in
      match a with
      | Double y -> Bool(x < y)::tl
      | _ -> push stack Error)
    | (Name m)::(Name n)::tl -> (
      let b = bind_list_search m bind_list parent_bind_list in
      let a = bind_list_search n bind_list parent_bind_list in
      match a,b with
      | Int x, Int y -> Bool(x < y)::tl
      | Double x, Double y -> Bool(x < y)::tl
      | _ -> push stack Error)
    | _ -> push stack Error in


  (* The bind command binds a name to a value. It is evaluated by popping two values from the stack. The second 
      value popped must be a name. The name is bound to the value (the first thing popped off the stack). The value
      can be an Int, Double, String, Bool, Unit, or value of a Name. The name value binding is stored in bind_list. 
      The result of a bind operation is :unit: which is pushed onto the stack. The value :error: will be pushed onto 
      the stack if we try to bind an identifier to an unbound identifier or if the stack is empty. *)
  let bind (scope : (const list * (string*const) list)) (parent_scope_list : (const list * (string*const) list) list) : (const list * (string*const) list) =
    let (stack, bind_list) = scope in 
    match stack with
    | y::Name(x)::tl -> (
      match y with
      | Int i -> (Unit::tl, (x,y)::bind_list)
      | Double d -> (Unit::tl, (x,y)::bind_list)
      | String s -> (Unit::tl, (x,y)::bind_list)
      | Bool b -> (Unit::tl, (x,y)::bind_list)
      | Unit -> (Unit::tl, (x,y)::bind_list)
      | Name n -> 
        (* call bind_list_search to find the value of the binding identifier *)
        let a = bind_list_search n bind_list parent_scope_list in
        (* if no value is found, push an Error onto the stack *)
        if a = Error then
          (push stack Error, bind_list)
        (* If a the binding identifier is in the bind_list, we add it's value to the newer binding identifier. The
            neat things about this is that we aren't actually updating the binding identifiers value, we just add another
            reference to it onto the list, BUT because we're searching top down we will always reference the newer binding first. 
            Might come back later to do an acutal update to the binding identifier to conserve memory of it becomes an issue.*)
        else
          (Unit::tl, (x,a)::bind_list)
      | Error -> (push stack Error, bind_list)
      )
    | _ -> (push stack Error, bind_list) in


  (* The if command pops three values off the stack: x, y and z. The third value popped (z, in this case)
      must always be a boolean. If z is :true:, executing the if command will push x back onto the stack,
      and if z is :false:, executing the if will push y back onto the stack. Pushes an error if the third
      value isn't a boolean, if the stack is empty, or if there are less than three values on the stack. *)
  let if_com (stack : const list) bind_list : const list =
    match stack with
    | x::y::(Bool z)::tl -> (
      if z then
        x::tl
      else
        y::tl)
    (* Adding binding handling for if command *)
    | x::y::(Name n)::tl -> (
      let a = bind_list_search n bind_list [] in
      match a with
      | Bool z -> (
        if z then
          x::tl
        else
          y::tl)
      | _ -> push stack Error)
    | _ -> push stack Error in


  let let_com (scope : (const list * (string*const) list)) (parent_scope_list : (const list * (string*const) list) list) : (const list * (string*const) list) list =
    scope::parent_scope_list in


  let end_com (scope : (const list * (string*const) list)) (parent_scope_list : (const list * (string*const) list) list) : (const list * (string*const) list)*(const list * (string*const) list) list =
    match parent_scope_list with
    | parent_scope::tl ->(
      let (stack, bind_list) = scope in
      let (parent_stack, parent_bind_list) = parent_scope in
      match stack with
      | x::xs -> ((x::parent_stack, parent_bind_list), tl)
      | [] -> ((parent_stack, parent_bind_list), tl))
    | [] -> (scope, parent_scope_list) in


  (* to_string removes the top element of the stack and converts it to a string. If the
      stack is empty, push an error. *)
  let to_string (stack : const list) bind_list : const list =
    match stack with
    | (Int i)::tl -> String(string_of_int i)::tl
    | (Double d)::tl -> String(string_of_float d)::tl
    | (Bool b)::tl ->
      if b then
        (String ":true:")::tl
      else
        (String ":false:")::tl
    | Error::tl -> (String ":error:")::tl
    | Unit::tl -> (String ":unit:")::tl
    | (String s)::tl -> (String s)::tl
    | (Name n)::tl -> (String n)::tl
    | _ -> push stack Error in


  (* println pops a string off the top of the stack and writes it, follwed by a newline,
      to thhe output file. Top element must be a string or an error will be pushed. Therefore
      either a string is at the top of the stack or to_string has been called previously. *)
  let println (stack : const list) (oc : out_channel) bind_list : const list =
    match stack with
    | (String s)::tl -> output_string oc (s ^ "\n"); tl
    | _ -> push stack Error in


  (* to_command converts string literals of command values to their corresponding command *)
  let to_command (s : string) : com =
    if String.length s >= 5 && String.sub s 0 5 = "push " then
      let x = String.sub s 5 (String.length s - 5) in
      Push (push_const x)
    else
      match s with
      | "pop" -> Pop
      | "add" -> Add
      | "sub" -> Sub
      | "mul" -> Mul
      | "div" -> Div
      | "rem" -> Rem
      | "neg" -> Neg
      | "swap" -> Swap
      | "cat" -> Cat
      | "and" -> And
      | "or" -> Or
      | "not" -> Not
      | "equal" -> Equal
      | "lessThan" -> LessThan
      | "bind" -> Bind
      | "if" -> If
      | "let" -> Let
      | "end" -> End
      | "toString" -> ToString
      | "println" -> Println
      | _ -> Quit in


  (* run_command effectively runs the code. It takes the current stack, the program sequence, and the output channel.
      It iterates through the program and runs each command until the program is exhausted (at which point quit is called
      and the final stack is returned). *)
  let rec run_command (scope : (const list * (string*const) list)) (parent_scope_list : (const list * (string*const) list) list) (program : prog) oc : (const list * (string*const) list) =
    match program with
    | [] -> scope
    | com::tl ->
      match com with
      | Quit -> scope
      | Push p -> 
        let (stack, bind_list) = scope in
        let new_scope = (push stack p, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Pop -> 
        let (stack, bind_list) = scope in
        let new_scope = (pop stack, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Add -> 
        let (stack, bind_list) = scope in
        let new_scope = (add stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Sub -> 
        let (stack, bind_list) = scope in
        let new_scope = (sub stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Mul -> 
        let (stack, bind_list) = scope in
        let new_scope = (mul stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Div -> 
        let (stack, bind_list) = scope in
        let new_scope = (div stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Rem -> 
        let (stack, bind_list) = scope in
        let new_scope = (rem stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Neg -> 
        let (stack, bind_list) = scope in
        let new_scope = (neg stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Swap -> 
        let (stack, bind_list) = scope in
        let new_scope = (swap stack bind_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Cat -> 
        let (stack, bind_list) = scope in
        let new_scope = (cat stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | And -> 
        let (stack, bind_list) = scope in
        let new_scope = (and_com stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Or -> 
        let (stack, bind_list) = scope in
        let new_scope = (or_com stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Not -> 
        let (stack, bind_list) = scope in
        let new_scope = (not_com stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Equal -> 
        let (stack, bind_list) = scope in
        let new_scope = (equal_com stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | LessThan -> 
        let (stack, bind_list) = scope in
        let new_scope = (lessThan_com stack bind_list parent_scope_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Bind ->
        let new_scope = bind scope parent_scope_list in
        run_command new_scope parent_scope_list tl oc 
      | If -> 
        let (stack, bind_list) = scope in
        let new_scope = (if_com stack bind_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Let -> 
        let new_parent_scope_list = let_com scope parent_scope_list in
        run_command ([],[]) new_parent_scope_list tl oc
      | End -> 
        let (new_scope, new_parent_scope_list) = end_com scope parent_scope_list in
        run_command new_scope new_parent_scope_list tl oc
      | ToString -> 
        let (stack, bind_list) = scope in
        let new_scope = (to_string stack bind_list, bind_list) in
        run_command new_scope parent_scope_list tl oc
      | Println -> 
        let (stack, bind_list) = scope in
        let new_scope = (println stack oc bind_list, bind_list) in
        run_command new_scope parent_scope_list tl oc in


  (* program is the List of commands to be executed. This is done through List.map which creates a prog
      by applying to_command to each string in ls_str, which was created through the pangram code chunk at
      lines 16-23. *)
  let program : prog = List.map to_command ls_str in


  let stack : const list = [] in


  let bind_list : (string*const) list = [] in

  
  let scope : (const list * (string*const) list) = (stack, bind_list) in


  let parent_scope_list : (const list * (string*const) list) list = [] in


  (* Run the interpreter *)
  let _ = run_command scope parent_scope_list program oc in


  (* Close our channels *)
  close_in ic;
  close_out oc;;


(*interpreter ("input.txt", "output.txt");;*)

