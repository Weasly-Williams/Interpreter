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


  (* add refers to integer OR floating point addition. Since this is a binary operator, it consumes
      the top two values in the stack, calculates the sum, and pushes the result back onto the stack. Only
      works with a combination of 2 integer and floating point values *)
  let add (stack : const list) bind_list : const list =
    match stack with
    | (Int y)::(Int x)::tl -> (Int (x + y))::tl
    | (Double y)::(Double x)::tl -> (Double (x +. y))::tl
    | (Int y)::(Double x)::tl -> (Double ((x +. float_of_int y)))::tl
    | (Double y)::(Int x)::tl -> (Double (float_of_int x +. y))::tl
    | _ -> push stack Error in


  (* sub refers to integer OR floating point subtraction. Works like add*)
  let sub (stack : const list) bind_list : const list =
    match stack with
    | (Int y)::(Int x)::tl -> (Int (x - y))::tl
    | (Double y)::(Double x)::tl -> (Double (x -. y))::tl
    | (Double y)::(Int x)::tl -> (Double ((float_of_int x -. y)))::tl
    | (Int y)::(Double x)::tl -> (Double (x -. float_of_int y))::tl
    | _ -> push stack Error in


  (* mul refers to integer OR floating point multiplication. Works like add and sub *)
  let mul (stack : const list) bind_list : const list =
    match stack with
    | (Int y)::(Int x)::tl -> (Int (x * y))::tl
    | (Double y)::(Double x)::tl -> (Double (x *. y))::tl
    | (Int y)::(Double x)::tl -> (Double ((x *. float_of_int y)))::tl
    | (Double y)::(Int x)::tl -> (Double (float_of_int x *. y))::tl
    | _ -> push stack Error in


  (* div refers to integer OR floating point division. Works like add, sub, and mul. We
      have to account for division by zero *)
  let div (stack : const list) bind_list : const list =
    match stack with
    | (Int 0)::tl -> push stack Error
    | (Double 0.0)::tl -> push stack Error
    | (Int y)::(Int x)::tl -> (Int (x / y))::tl
    | (Double y)::(Double x)::tl -> (Double (x /. y))::tl
    | (Int y)::(Double x)::tl -> (Double ((x /. float_of_int y)))::tl
    | (Double y)::(Int x)::tl -> (Double (float_of_int x /. y))::tl
    | _ -> push stack Error in


  (* rem refers to the remainder of integer division. ONLY applies to integers in this case.
      Works like div. *)
  let rem (stack : const list) bind_list : const list =
    match stack with
    | (Int 0)::tl -> push stack Error
    | (Int y)::(Int x)::tl -> (Int (x mod y))::tl
    | _ -> push stack Error in

  
  (* neg calculates the negation of an integer OR double. Negation of 0 is still 0. *)
  let neg (stack : const list) bind_list : const list =
    match stack with
    | (Int x)::tl -> (Int (-x))::tl
    | (Double x)::tl -> (Double (-.x))::tl
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
  let cat (stack : const list) bind_list : const list =
    match stack with
    | (String y)::(String x)::tl -> String (x ^ y)::tl
    | _ -> push stack Error in


  (* The command and performs the logical conjunction of the top two elements in the stack
      and pushes the result (a single value) on to the stack. Will push an error if there is 
      only one element on the stack, the stack is empty, either of the top two elements aren't 
      booleans. *)
  let and_com (stack : const list) bind_list : const list =
    match stack with
    | (Bool y)::(Bool x)::tl -> Bool(x && y)::tl
    | _ -> push stack Error in


  (* The command or performs the logical disjunction of the top two elements in the stack and 
      pushes the result (a single value) onto the stack. Pushes an erro if there is only one element
      on the stack, the stack is empty, either of the top two elements aren't booleans. *)
  let or_com (stack : const list) bind_list : const list = 
    match stack with
    | (Bool y)::(Bool x)::tl -> Bool(x || y)::tl
    | _ -> push stack Error in


  (* The command not p erforms the logical negation of the top element in the stack and pushes the 
      result (a single value) onto the stack. Since the op erator is unary, it only consumes the top 
      value from the stack. Pushes an error if the stack is empty or the top element isn't a boolean. *)
  let not_com (stack : const list) bind_list : const list = 
    match stack with
    | (Bool x)::tl -> Bool(not x)::tl
    | _ -> push stack Error in


  (* The command equal refers to numeric equality (so you are not supporting string comparisons). The
      command should handle both Integer as well as floating point values. However, an Integer and a
      Double are not to be treated the same. For example, 7 and 7.0 are NOT comparable. This operator 
      consumes the top two values on the stack and pushes the result (a single boolean value) onto the stack. 
      Pushes an error if only one element in the stack, stack is empty, if either of the top two elements aren't 
      both Integers OR both Doubles. *)
  let equal_com (stack : const list) bind_list : const list = 
    match stack with
    | (Int y)::(Int x)::tl -> Bool(x = y)::tl 
    | (Double y)::(Double x)::tl -> Bool(x = y)::tl
    | _ -> push stack Error in


  (* The command lessThan refers to numeric less than ordering. This operator consumes the top two 
      values on the stack and pushes the result (a single boolean value) onto the stack. The command 
      should handle b oth Integer as well as floating point values. However, an Integer and a Double 
      are not to be treated the same. For example, 7 and 7.0 are NOT comparable. Push an error if there 
      is only one element, the stack is empty, or if the top two elements aren't both Integers OR both
      Doubles. *)
  let lessThan_com (stack : const list) bind_list : const list = 
    match stack with
    | (Int y)::(Int x)::tl -> Bool(x < y)::tl
    | (Double y)::(Double x)::tl -> Bool(x < y)::tl
    | _ -> push stack Error in


  (* bind_list_search is a helper function that takes a name and the bind list. We compare the 
      name binding of the head of the bind list with the parameter name. If they are equal, return
      the associated value. Else continue searching the bind list for a matching name. If no name is
      found, then return an Error. *)
  let rec bind_list_search (name : string) (bind_list : (string*const) list) : const = 
    match bind_list with
    | (s,x)::tl ->
      if name = s then
        x
      else
        bind_list_search name tl 
    | [] -> Error in


  let bind (stack : const list) (bind_list : (string*const) list) : const list * (string*const) list = 
    match stack with
    | y::Name(x)::tl -> (
      match y with
      | Int i -> (Unit::tl, (x,y)::bind_list)
      | Double d -> (Unit::tl, (x,y)::bind_list)
      | String s -> (Unit::tl, (x,y)::bind_list)
      | Bool b -> (Unit::tl, (x,y)::bind_list)
      | Unit -> (Unit::tl, (x,y)::bind_list)
      | Name n -> 
          let a = bind_list_search n bind_list in
          if a = Error then
            (push stack Error, bind_list) 
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
    | _ -> push stack Error in


  let let_com (stack : const list) bind_list : const list = [] in


  let end_com (stack : const list) bind_list : const list = [] in

  
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
  let rec run_command (stack : const list) (program : prog) (bind_list : (string*const) list) oc : const list * (string*const) list =
    match program with
    | [] -> (stack, bind_list)
    | com::tl ->
      match com with
      | Quit -> (stack, bind_list)
      | Push p -> run_command (push stack p) tl bind_list oc
      | Pop -> run_command (pop stack) tl bind_list oc
      | Add -> run_command (add stack bind_list) tl bind_list oc
      | Sub -> run_command (sub stack bind_list) tl bind_list oc
      | Mul -> run_command (mul stack bind_list) tl bind_list oc
      | Div -> run_command (div stack bind_list) tl bind_list  oc
      | Rem -> run_command (rem stack bind_list) tl bind_list oc
      | Neg -> run_command (neg stack bind_list) tl bind_list oc
      | Swap -> run_command (swap stack bind_list) tl bind_list oc
      | Cat -> run_command (cat stack bind_list) tl bind_list oc
      | And -> run_command (and_com stack bind_list) tl bind_list oc
      | Or -> run_command (or_com stack bind_list) tl bind_list oc
      | Not -> run_command (not_com stack bind_list) tl bind_list oc
      | Equal -> run_command (equal_com stack bind_list) tl bind_list oc
      | LessThan -> run_command (lessThan_com stack bind_list) tl bind_list oc
      | Bind -> 
        let (updated_stack, updated_bind_list) = bind stack bind_list in
        run_command updated_stack tl updated_bind_list oc
      | If -> run_command (if_com stack bind_list) tl bind_list oc
      | Let -> run_command (let_com stack bind_list) tl bind_list oc
      | End -> run_command (end_com stack bind_list) tl bind_list oc
      | ToString -> run_command (to_string stack bind_list) tl bind_list oc
      | Println -> run_command (println stack oc bind_list) tl bind_list oc in


  (* program is the List of commands to be executed. This is done through List.map which creates a prog
      by applying to_command to each string in ls_str, which was created through the pangram code chunk at
      lines 16-23. *)
  let program : prog = List.map to_command ls_str in


  let bind_list : (string*const) list = [] in


  (* Run the interpreter *)
  let _ = run_command [] program bind_list oc in


  (* Close our channels *)
  close_in ic;
  close_out oc;;


interpreter ("input.txt", "output.txt");;