(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Less | Greater |
          And | Or  | Link | RevLink | BiLink(* | Arrow | Colon *)

type uop = Neg | Not

type expr =
    Literal of int
  | FLit of string
  | BoolLit of bool
  | ChrLit of char
  | StrLit of string
  | Id of string
  | NodeLit of string * expr
  | GraphLit of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr * expr
  | Call of string * expr list
  | Attr of string * string * expr * expr
  | Access of string * expr
  | EdgeList of expr * expr list
  | EdgeOp of expr * expr * op * expr * expr
  | EdgeOpBi of expr * expr * op * expr * expr * expr
  | NodeAttr of expr * string * expr
  | Noexpr

type typ = Void | Int | String | Float | Bool | Char | Array of typ * expr
         | Node | Edge| Graph 
 
type bind = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | Bfs of expr * expr * expr * stmt
  | While of expr * stmt
  | Binding of bind      (* Only for vdecls *)
  | Binding_Assign of bind * expr
  | Return of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = stmt list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Less -> "<"
  | Greater -> ">"
  | And -> "&&"
  | Or -> "||"
  | Link -> "->"
  | RevLink -> "<-"
  | BiLink -> "<->"
  (*
  | Arrow -> "->"
  | Colon -> ":"
  *)

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | NodeLit(_, name) -> string_of_expr name
  | GraphLit(name) -> name
  | StrLit(str) -> str
  | ChrLit(c) -> Char.escaped c
  | Id(s) -> s
  | NodeAttr(e1, e2, e3) ->
      string_of_expr e1 ^ " " ^ e2 ^ " " ^ string_of_expr e3
  | Attr(s, a, e, e2) -> if e = Noexpr then 
                                s ^ "." ^ a 
                         else ( if e2 = Noexpr then
                                 s ^ "." ^ a ^ "[" ^ string_of_expr e ^ "]"
                                else
                                 s ^ "." ^ a ^ "[" ^ string_of_expr e ^ ", " ^ string_of_expr e2 ^ "]"
                         )
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e1, e2) -> 
      (match e2 with 
         Noexpr -> v ^ " = " ^ string_of_expr e1
       | _ -> v ^ "[" ^ string_of_expr e1 ^ "]" ^ " = " ^ string_of_expr e2 
      )
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Access(id, e) -> id ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""
  | EdgeOpBi(_, e1, o, e3, e4, e5) -> string_of_expr e1 ^ " "^ string_of_expr e5 ^ string_of_op o ^ "|" 
        ^ string_of_expr e3 ^ "| " ^ string_of_expr e4
  | EdgeOp(_, e1, o, e3, e4) -> string_of_expr e1 ^ " " ^ string_of_op o ^ "|" 
        ^ string_of_expr e3 ^ "| " ^ string_of_expr e4
  | EdgeList(e1, e2) -> string_of_expr e1 ^ ": " 
        ^ (List.fold_left (fun s e -> s ^ match e with
                EdgeOp(_, e1, o, e3, e4) -> (match o with
                        Link -> (string_of_expr e1 ^ " " ^ string_of_op o ^ "|" 
                         ^ string_of_expr e3 ^ "| ")
                      | _ -> "[" ^ string_of_op o ^ " " ^ string_of_expr e4 ^ "],  ")
              | EdgeOpBi(_, e1, o, e3, _, e5) ->
                        (string_of_expr e1 ^ " |" ^ string_of_expr e5 ^ "|" ^ string_of_op o ^ "|" 
                         ^ string_of_expr e3 ^ "| ")
              | _ -> ""
        ) "" (List.rev e2))
        ^ match (List.hd e2) with
                EdgeOp(_, _, _, _, e4) -> string_of_expr e4
              | EdgeOpBi(_, _, _, _, e4, _) -> string_of_expr e4
              | _ -> ""
  (*| NodeOfGraph(e1, e2) -> "(" ^  e1 ^ ", " 
    ^ e2 ^ ")"
  *)

let rec string_of_typ = function
    Void        -> "void"
  | Int         -> "int"
  | Float       -> "float"
  | String      -> "String"
  | Bool        -> "bool"
  | Char        -> "char"
  | Node        -> "Node"
  | Graph       -> "Graph"
  | Edge        -> "Edge"
  | Array(t, e) -> string_of_typ t ^ "[" ^ string_of_expr e ^ "]"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Bfs(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Binding(t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n" 
  | Binding_Assign((t, id), e) -> 
        match e with
                Assign(_, e',_) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e' ^ ";\n"
                | _           -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e  ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

