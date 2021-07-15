(* Semantic checking for the YAGL compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement *)

let check (stmts, funcs) =
  let main = 
     {
       typ = Int;
       fname = "main"; 
       formals = [];
       body = stmts
     }

   in

  let functions = main :: funcs

  in

 (* Extract the statements that are vdecls *)
  let extract_vdecls (stmts : stmt list) =

    List.fold_left (fun bind_list stmt -> 
      match stmt with
        Binding b -> b :: bind_list
      | Binding_Assign (b, _) -> b :: bind_list
      | _ -> bind_list 
    ) [] stmts

  in

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_stmt_binds (kind : string) (stmts : stmt list) =

    let binds = extract_vdecls stmts

    in

    List.iter (function
    (Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (* Verify a list of bindings has no void types or duplicate names *)
  (* Verify also that size of array is type int *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
        (Void, b)        -> raise(Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      | ((_,n1) :: (_,n2) :: _) when n1 = n2 ->
    raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in


  (**** Check variables declared in statements ****)

  check_stmt_binds "stmts" stmts;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Void);
                                                 ("printInt", Int); 
                                                 ("printString", String);
                                                 ("printBool", Bool);
                                                 ("printFloat", Float);
                                                 ("printChar", Char);
                                                 ("printNode", Node);
                                                 ("printGraph", Graph)
                                               ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and main_err = "reserved function name: " ^ fd.fname ^ " cannot be used"
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> 
                       if compare n "main" == 0
                        then  make_err main_err
                        else make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  
let check_function func =
    
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;

    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in  

    (* Build local symbol table of variables for this function *)
    let arg_symbols = [List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (func.formals)]
    in

    (* Return a variable from our local symbol table *)
    let rec type_of_identifier s symbol_tables_list = match symbol_tables_list with
       last_map :: []        -> (try StringMap.find s last_map with Not_found -> raise (Failure ("undeclared identifier " ^ s)))
     | head_map :: tail_maps -> (try StringMap.find s head_map with Not_found -> type_of_identifier s tail_maps)
     | []                    -> raise(Failure("Internal Error: Symbol table not built."))
    in

    let type_of_attribute a = match a with
        "visited"   -> Bool
      | "curr_dist" -> Int
      | "length" -> Int
      | "name"   -> String
      | "num_nodes" -> Int
      | "num_neighbors" -> Int
      | "node" -> Node
      | "neighbor" -> Node
      | "weight" -> Int
      | _ -> raise( Failure "Unknown attribute!")
    in
    (* Check array sizes are all of type int *)
    let check_arrays s_table (_ : string) (binds : bind list) =
       List.iter (function
           (Array(_, e), _) -> let rec is_int e' = match e' with 
                                 Literal(_)        -> true
                               | Binop(e1, _, e2) -> if (is_int e1) && (is_int e2) then true else false
                               | Id(s)             -> let typ = type_of_identifier s s_table in 
                                                                (match typ with 
                                                                  Int -> true
                                                                | _   -> false) 
                               | _                 -> false
                               in let found_int = is_int e
                               in if found_int then () else raise(Failure("Size of array is not of type int."))
         | _ -> ()) binds;
    in check_arrays arg_symbols "formals" func.formals;
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr ex1 s_table = match ex1 with
         Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else if compare fname "main" == 0 then
            raise (Failure ("Cannot call main otherwise recurse forever"))
          else let check_call (ft, _) e = 
            let (et, e') = expr e s_table in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (if fname = "print" then  (et, e') else (check_assign ft et err, e'))
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
       | Literal  l -> (Int, SLiteral l)
       | FLit f -> (Float, SFLit f)
       | ChrLit c -> (Char, SChrLit c)
       | NodeLit (n, name) -> let name' = expr name s_table in 
                (match name' with
                        (String, _) -> (Node, SNodeLit (n, expr name s_table))
                        | _ -> raise (Failure "Node must be passed a String")
                )
       | GraphLit e -> (Graph, SGraphLit e)
       | StrLit s -> (String, SStrLit s)
       | EdgeList(e1, e2) ->
          let (t1, e1') = expr e1 s_table in
          let e2' = List.map (fun ele ->
                  match ele with
                        EdgeOp(_,y,z,q,r) -> expr (EdgeOp(e1, y,z,q,r)) s_table
                      | EdgeOpBi(_,y,z,q,r,x) -> expr (EdgeOpBi(e1, y,z,q,r,x)) s_table
                      | _ -> raise (Failure "ERROR in semant for edgelist. Illegal inner types.")
              ) e2 in
          let ty = match t1 with
                Graph -> Graph
              | _ -> raise(Failure ("illegal graph operator: " 
                                    ^ string_of_typ t1  ^ " " 
                                    ^ string_of_expr e1 
                                    ^ ". Was expecting type Graph"))
          in
          (ty, SEdgeList((t1,e1'), e2'))
       | EdgeOpBi(e1, e2, op, e3, e4,e5) as e ->
          let (t1, e1') = expr e1 s_table
          and (t2, e2') = expr e2 s_table
          and (t3, e3') = expr e3 s_table
          and (t5, e5') = expr e5 s_table
          and (t4, e4') = expr e4 s_table in
          let ty = match op with
            Link when t2 = Node && t3 = Int && t4 = Node && t5 = Int -> Graph
          | RevLink when t2 = Node && t3 = Int && t4 = Node  && t5 = Int -> Graph
          | BiLink when t2 = Node && t3 = Int && t4 = Node  && t5 = Int -> Graph
          | Sub  when t2 = Node && t3 = Int && t4 = Node  && t5 = Int -> Graph
          | Add  when t2 = Node && t3 = Int && t4 = Node  && t5 = Int -> Graph
          | _ -> raise (
	      Failure ("illegal graph operator " ^
                       string_of_typ t2 ^ " " ^ 
                       string_of_op op ^ "{" ^ string_of_typ t3 ^ "} " ^
                       string_of_typ t4 ^ " in " ^ string_of_expr e)) 
          in (ty, SEdgeOpBi((t1, e1'), (t2, e2'),  op, (t3, e3'), (t4, e4'), (t5, e5')))
       | EdgeOp(e1, e2, op, e3, e4) as e ->
          let (t1, e1') = expr e1 s_table
          and (t2, e2') = expr e2 s_table
          and (t3, e3') = expr e3 s_table
          and (t4, e4') = expr e4 s_table in
          let ty = match op with
            Link when t2 = Node && t3 = Int && t4 = Node -> Graph
          | RevLink when t2 = Node && t3 = Int && t4 = Node -> Graph
          | BiLink when t2 = Node && t3 = Int && t4 = Node -> Graph
          | Sub  when t2 = Node && t3 = Int && t4 = Node -> Graph
          | Add  when t2 = Node && t3 = Int && t4 = Node -> Graph
          | _ -> raise (
	      Failure ("illegal graph operator " ^
                       string_of_typ t2 ^ " " ^ 
                       string_of_op op ^ "{" ^ string_of_typ t3 ^ "} " ^
                       string_of_typ t4 ^ " in " ^ string_of_expr e)) 
          in (ty, SEdgeOp((t1, e1'), (t2, e2'),  op, (t3, e3'), (t4, e4')))
       | Id s       -> (type_of_identifier s s_table, SId s)
       | NodeAttr(e1, e2, e3) as e ->
          let (t1, e1') = expr e1 s_table
          and t2 = type_of_attribute e2
          and (t3, e3') = expr e3 s_table in
          let ty = match t2 with
            Bool when t3 = Bool && t1 = Node -> Void
          | Int when t3 = Int && t1 = Node -> Void
          | _ -> raise (
              Failure ("illegal visit operands " ^
                       string_of_typ t1 ^ " " ^
                       string_of_typ t3 ^ " in " ^ string_of_expr e))
          in (ty, SNodeAttr((t1, e1'), t2, (t3, e3')))
       | Attr(s, a, e, e2) -> let e' = expr e s_table in
                          let e2' = expr e2 s_table in
                          let et = (match e' with (t, _) -> t) in
                          let e2t = (match e2' with (t, _) -> t) in
                          let st = type_of_identifier s s_table in
                          let err = "Wrong accessor type, [" ^ string_of_typ et ^ ", " ^ string_of_typ e2t ^"], on attribute " ^ a ^ "." in
                          let ret = (type_of_attribute a, SAttr ((type_of_identifier s s_table, SId s), a, e', e2')) in
                          (match a with
                                  "length"              -> if st = String && et = Void && e2t = Void then ret else raise (Failure err)
                                | "name"                -> if st = Node   && et = Void && e2t = Void then ret else raise (Failure err)
                                | "num_nodes"           -> if st = Graph  && et = Void && e2t = Void then ret else raise (Failure err)
                                | "num_neighbors"       -> if st = Graph  && et = Node && e2t = Void then ret else raise (Failure err)
                                | "node"                -> if st = Graph  && et = Int  && e2t = Void then ret else raise (Failure err)
                                | "neighbor"            -> if st = Graph  && et = Node && e2t = Int  then ret else raise (Failure err)
                                | "curr_dist"           -> if st = Node   && et = Void && e2t = Void then ret else raise (Failure err)
                                | "visited"             -> if st = Node   && et = Void && e2t = Void then ret else raise (Failure err)
                                | "weight"              -> if st = Graph  && et = Node && e2t = Node then ret else raise (Failure err)
                                | _                     -> raise (Failure err))
       | Binop(e1, op, e2) as e -> 
          let (t1, e1') = expr e1 s_table 
          and (t2, e2') = expr e2 s_table in
          (* All binary operators require operands of the same type *)
          let same = t1 = t2 in
          (* Determine expression type based on operator and operand types *)
          let ty = match op with
            Add | Sub | Mult | Div when same && t1 = Int   -> Int
          | Add when same && t1 = String   -> String
          | Add | Sub  when t1 = Graph && t2 = Node -> Graph
          | Add | Sub | Mult | Div when same && t1 = Float -> Float
          | Equal                  when same               -> Bool
          | Less | Greater
                     when same && (t1 = Int || t1 = Float) -> Bool
          | And | Or when same && t1 = Bool -> Bool
          | _ -> raise (
	      Failure ("illegal binary operator " ^
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e))
          in (ty, SBinop((t1, e1'), op, (t2, e2')))       
        | Assign(var, e1, e2) as ex -> 
          let (rvalue, lt) = match e2 with 
              Noexpr -> (e1, type_of_identifier var s_table)
            | _      -> let elem_typ = type_of_identifier var s_table in 
                        ( match elem_typ  with 
                            Array(t, e) -> (match (e1, e) with
                                           (Literal index, Literal arr_size) -> 
                                                           if index > (arr_size - 1) 
                                                           then raise(Failure("ERROR: Index out of bounds.")) 
                                                           else (e2, t)
                                            | (Unop _, _)  -> raise(Failure("ERROR: Index out of bounds.")) 
                                            | _            -> (e2, t)  
                                           )
                           | _ -> raise(Failure("ERROR: This case should not have been reached.")) 
                        )
          in
            let (rt, _) = expr rvalue s_table in
            let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ 
              string_of_typ rt ^ " in " ^ string_of_expr ex
            in (match lt with
                Node -> if rt = String then 
                        (Node, SAssignNode(var, expr e1 s_table, expr e2 s_table)) 
                    else 
                        (check_assign lt rt err, SAssign(var, expr e1 s_table, expr e2 s_table))
                | _ -> (check_assign lt rt err, SAssign(var, expr e1 s_table, expr e2 s_table)))
       | BoolLit b -> (Bool, SBoolLit b)
       | Access (s, e) -> 
         let elem_typ = type_of_identifier s s_table in 
         let e' = expr e s_table in 
         ( match elem_typ  with 
             Array(t, _) ->
                                     (match e' with 
                                       (Int, _) -> (t, SAccess(s, e'))
                                     | (_, _)   -> raise(Failure("Can only access array element with int type."))
                                     )
           | _ -> raise(Failure("ERROR: This case should not have been reached.")) 
         )
       | Noexpr -> (Void, SNoexpr) 
       (* Exprs still to implement below *) 
      | Unop(op, e) as ex -> 
          let (t, e') = expr e s_table in
          let ty = match op with
            Neg when t = Int || t = Float -> t
          | Not when t = Bool -> Bool
          | _ -> raise (Failure ("illegal unary operator " ^ 
                                 string_of_uop op ^ string_of_typ t ^
                                 " in " ^ string_of_expr ex))
          in (ty, SUnop(op, (t, e')))
    in

    let check_bool_expr e s_table = 
      let (t', e') = expr e s_table
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in 

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt stmt symbol_table = match stmt with
        Expr e -> SExpr (expr e symbol_table)
      | If(p, b1, b2) -> SIf(check_bool_expr p symbol_table, check_stmt b1 symbol_table, check_stmt b2 symbol_table)
      | Bfs(_, _, _, _) -> raise (Failure "fail for")
      | While(p, s) -> SWhile(check_bool_expr p symbol_table, check_stmt s symbol_table)
      | Return e -> let (t, e') = expr e symbol_table in
                if t = func.typ then SReturn (t, e')
                else raise (
                        Failure ("return gives " ^ string_of_typ t ^ ", but expected " ^
                                 string_of_typ func.typ ^ " in return " ^ string_of_expr e))
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | Block sl ->

          (* Create new symbol table for this block's scope and add to head of outer scope symbol table *)
          let updated_table = (List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty (extract_vdecls sl)) :: symbol_table 
          in  

          (* Make sure no formals or locals are void or duplicates *)
          check_stmt_binds "local" sl;
          (* Check array sizes are all of type int *)
          check_arrays updated_table "locals" (extract_vdecls sl);

          let rec check_stmt_list s m = match s with
              [Return _ as s] -> [check_stmt s m]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> let b1  = Block(sl) in  (check_stmt b1 m) :: (check_stmt_list ss m)
            | s :: ss         -> check_stmt s m :: check_stmt_list ss m
            | []              -> []

          in SBlock(check_stmt_list sl updated_table)
      | Binding (typ, id) -> 
                      if typ = Node then
                        SBinding_Assign ((typ, id), (typ, SNoexpr))
                      else
                        SBinding (typ, id)
      | Binding_Assign ((typ, id), e) -> 
                      SBinding_Assign ((typ, id), expr e symbol_table);
  in 

  { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      sbody = match check_stmt (Block func.body) arg_symbols with
  SBlock(sl) -> sl
      | _ -> raise (Failure ("internal error: block didn't become a block?"))
    }
  
  in (List.map check_function functions)
