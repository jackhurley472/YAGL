(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFLit of string
  | SBoolLit of bool
  | SChrLit of char
  | SStrLit of string
  | SId of string
  | SNodeLit of string * sexpr
  | SGraphLit of string
  | SBinop of sexpr * op * sexpr
  | SNodeAttr of sexpr * typ * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr * sexpr
  | SAssignNode of string * sexpr * sexpr
  | SCall of string * sexpr list
  | SAttr of sexpr * string * sexpr * sexpr
  | SAccess of string * sexpr
  | SEdgeList of sexpr * sexpr list
  | SEdgeOp of sexpr * sexpr * op * sexpr * sexpr
  | SEdgeOpBi of sexpr * sexpr * op * sexpr * sexpr * sexpr
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SBfs of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SBinding of bind  
  | SBinding_Assign of bind * sexpr
  | SReturn of sexpr 

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SFLit(l) -> l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SChrLit(c) -> Char.escaped c
  | SNodeLit(_, name)  -> string_of_sexpr name
  | SGraphLit(name) -> name
  | SStrLit(str) -> str
  | SId(s) -> s
  | SNodeAttr(e1, t, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_typ t ^ " " ^  string_of_sexpr e2 
  | SAttr(sx, a, e, e2) -> (match e with
                        (_, SNoexpr) | (Void, _) -> string_of_sexpr sx ^ "." ^ a 
                      | _ -> string_of_sexpr sx ^ "." ^ a ^ "[" ^ string_of_sexpr e ^ ", " ^ string_of_sexpr e2 ^ "]"  )
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e1, e2) | SAssignNode(v, e1, e2) -> 
      (match e2 with 
          (Void, SNoexpr) -> v ^ " = " ^ string_of_sexpr e1
        | _ -> v ^ "[" ^ string_of_sexpr e1 ^ "]" ^ " = " ^ string_of_sexpr e2 
      )
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SAccess(id, e) -> id ^ "[" ^ string_of_sexpr e ^ "]"
  | SNoexpr -> ""
  | SEdgeOpBi(_, e1, o, e3, e4, e5) -> string_of_sexpr e1 ^ " " ^ string_of_sexpr e5 ^ string_of_op o ^ "|"
        ^ string_of_sexpr e3 ^ "| " ^ string_of_sexpr e4
  | SEdgeOp(_, e1, o, e3, e4) -> string_of_sexpr e1 ^ " " ^ string_of_op o ^ "|"
        ^ string_of_sexpr e3 ^ "| " ^ string_of_sexpr e4
  | SEdgeList(e1, e2) -> string_of_sexpr e1 ^ ": " 
        ^ (List.fold_left (fun s e -> s ^ match e with
                (_, SEdgeOp(_, e2, o, e3, e4)) -> (match o with
                        Link -> (string_of_sexpr e2 ^ " " ^ string_of_op o 
                         ^ string_of_sexpr e3 ^ " ")
                      | _ -> "[" ^ string_of_op o ^ " " ^ string_of_sexpr e4 ^ "] ")
              |  (_, SEdgeOpBi(_, e2, o, e3, _, e5)) ->
                        (string_of_sexpr e2 ^ " " ^ string_of_sexpr e5 ^ string_of_op o 
                         ^ string_of_sexpr e3 ^ " ")
              | _ -> ""
        ) "" (List.rev e2))
        ^ match (List.hd e2) with
                (_, SEdgeOp(_, _, _, _, e4)) -> string_of_sexpr e4
              | (_, SEdgeOpBi(_, _, _, _, e4, _)) -> string_of_sexpr e4
              | _ -> ""
				  ) ^ ")"				     

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SBfs(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SBinding(t, id) -> "(" ^ string_of_typ t ^ " : " ^ id ^ ");\n"
  | SBinding_Assign((t, id), e) -> 
        string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr e ^ ";\n"
  | SReturn(e) -> "return " ^ string_of_sexpr e ^ ";\n" 

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (funcs) =
  String.concat "\n" (List.map string_of_sfdecl funcs)

