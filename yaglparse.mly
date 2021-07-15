/* Ocamlyacc parser for YAGL - Yet Another Graph Language 
   Forked from MicroC
*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAC RBRAC COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW REVARROW BIARROW COLON DOT
%token NOT EQ LT GT AND OR BAR
%token RETURN IF ELSE FOR WHILE INT BOOL FLOAT VOID CHAR STRING NODE GRAPH EDGE
%token <int> LITERAL
%token <bool> BLIT
%token <char> CHRLIT
%token <string> STRLIT
%token <string> ID FLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ COLON
%left LT GT
%left ARROW 
%left REVARROW 
%left BIARROW
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%left LBRAC 
%left COMMA
%left DOT

%%

program:
  src_file EOF { (List.rev (fst $1), snd $1) }

src_file:
   /* nothing */ { ([], [])               }
 | src_file stmt { (($2 :: fst $1), snd $1) }
 | src_file fdecl { (fst $1, ($2 :: snd $1)) }

typ:
    INT    { Int    }
  | FLOAT  { Float  }
  | STRING { String }
  | VOID   { Void   }
  | BOOL   { Bool   }
  | typ LBRAC expr RBRAC { Array($1, $3) }
  | NODE LBRAC expr RBRAC{ Array(Node, $3) }
  | EDGE   { Edge   }
  | CHAR   { Char   }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 body = List.rev $7 } }
    | GRAPH ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = Graph;
	 fname = $2;
	 formals = List.rev $4;
	 body = List.rev $7 } }

    | NODE ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = Node;
	 fname = $2;
	 formals = List.rev $4;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | NODE ID                  { [(Node, $2)]  }
  | GRAPH ID                 { [(Graph, $2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }
  | formal_list COMMA NODE ID { (Node,$4) :: $1 }
  | formal_list COMMA GRAPH ID { (Graph,$4) :: $1 }
  
stmt_list:
  /* nothing */    { [] }
  | stmt_list stmt { $2::$1 }

graph_stmts:
    NODE ID LPAREN expr RPAREN SEMI         { Binding_Assign((Node, $2), 
                                              Assign($2, NodeLit($2, $4), Noexpr))              }
  | NODE ID SEMI                            { Binding_Assign((Node, $2), 
                                              Assign($2, NodeLit($2, StrLit("")), Noexpr))      }
  | NODE ID ASSIGN expr SEMI                { Binding_Assign((Node, $2), 
                                              Assign($2, $4, Noexpr))                           }
  | GRAPH ID SEMI                           { Binding_Assign((Graph, $2),
                                              Assign($2, GraphLit($2), Noexpr))                 }


stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | graph_stmts                             { $1                    }
  | typ ID SEMI                             { Binding($1, $2)       }
  | typ ID ASSIGN expr SEMI                 { Binding_Assign(($1, $2), Assign($2,$4,Noexpr)) }

expr_opt: /* can be expr or nothing */
        /* epsilon/nothing */   { Noexpr }
      | expr                    { $1     }

biarrow:
    ID BIARROW ID                       { [EdgeOpBi(Noexpr, Id($1), BiLink, Literal(1), Id($3), Literal(1))] }
  | ID BIARROW lit_id_expr ID           { [EdgeOpBi(Noexpr, Id($1), BiLink, $3, Id($4), Literal(1))]     }
  | ID lit_id_expr BIARROW lit_id_expr ID           
                                        { [EdgeOpBi(Noexpr, Id($1), BiLink, $4, Id($5), $2)]             }
  | ID lit_id_expr BIARROW ID           
                                        { [EdgeOpBi(Noexpr, Id($1), BiLink, Literal(1), Id($4), $2)]     }
  | ID LBRAC BIARROW lit_id_expr RBRAC ID           
                                        { [EdgeOpBi(Noexpr, Id($1), BiLink, $4, Id($6), Literal(1))]     }
  | ID LBRAC lit_id_expr BIARROW lit_id_expr RBRAC ID           
                                        { [EdgeOpBi(Noexpr, Id($1), BiLink, $5, Id($7), $3)]             }
  | ID LBRAC lit_id_expr BIARROW RBRAC ID           
                                        { [EdgeOpBi(Noexpr, Id($1), BiLink, Literal(1), Id($6), $3)]     }

edge:
    edge COMMA edge                      { $3 @ $1                                                }
  /* Base Cases */
  | ID ARROW lit_id_expr ID             { [EdgeOp(Noexpr, Id($1), Link, $3, Id($4))]             }
  | ID ARROW ID                         { [EdgeOp(Noexpr, Id($1), Link, Literal(1), Id($3))]     }

  | ID REVARROW ID                      { [EdgeOp(Noexpr, Id($1), RevLink, Literal(1), Id($3))]  }
  | ID REVARROW lit_id_expr ID          { [EdgeOp(Noexpr, Id($1), RevLink, $3, Id($4))]          }
  | ID lit_id_expr REVARROW ID          { [EdgeOp(Noexpr, Id($1), RevLink, $2, Id($4))]          }
  | ID LBRAC lit_id_expr REVARROW RBRAC ID { [EdgeOp(Noexpr, Id($1), RevLink, $3, Id($6))]       }

  | biarrow                             { $1 }

  | PLUS ID                             { [EdgeOp(Noexpr, Id($2), Add, Literal(1), Id($2))]      }

  | MINUS ID                            { [EdgeOp(Noexpr, Id($2), Sub, Literal(1), Id($2))]      }

  /* Recursive Cases */
  | edge PLUS ID                        { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          Add, Literal(1), Id($3))  :: $1                        } 

  | edge MINUS ID                       { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          Sub, Literal(1), Id($3))  :: $1                        } 

  | edge ARROW ID                       { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")),
                                          Link, Literal(1), Id($3)) :: $1                        }
  | edge ARROW lit_id_expr ID           { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          Link, $3, Id($4))  :: $1                               } 

  | edge REVARROW ID                    { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")),
                                          RevLink, Literal(1), Id($3)) :: $1                     }
  | edge REVARROW lit_id_expr ID        { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          RevLink, $3, Id($4))  :: $1                            } 
  | edge LBRAC REVARROW lit_id_expr RBRAC ID        
                                        { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          RevLink, $4, Id($6))  :: $1                            } 
  | edge LBRAC lit_id_expr REVARROW RBRAC ID        
                                        { EdgeOp(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          RevLink, Literal(1), Id($6))  :: $1                    } 

  | edge BIARROW ID                     { EdgeOpBi(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")),
                                          BiLink, Literal(1), Id($3), Literal(1)) :: $1                      }
  | edge BIARROW lit_id_expr ID         { EdgeOpBi(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          BiLink, $3, Id($4), Literal(1))  :: $1                             }
  | edge LBRAC lit_id_expr BIARROW lit_id_expr RBRAC ID         
                                        { EdgeOpBi(Noexpr, (match (List.hd($1)) with
                                                EdgeOp(_,_,_,_,x) -> x
                                              | EdgeOpBi(_,_,_,_,x,_) -> x
                                              | _ -> raise (Failure "Error parsing edges.")), 
                                          BiLink, $5, Id($7), $3)  :: $1                             }

  lit_id_expr:
          ID                            { Id($1) }
        | LITERAL                       { Literal($1) }
        | BAR expr BAR                  { $2 }

expr:
    LITERAL          { Literal($1)                       }
  | FLIT             { FLit($1)                          }
  | BLIT             { BoolLit($1)                       }
  | CHRLIT           { ChrLit($1)                        }
  | STRLIT           { StrLit($1)                        }
  | ID               { Id($1)                            }
  | expr PLUS   expr { Binop($1, Add,   $3)              }
  | expr MINUS  expr { Binop($1, Sub,   $3)              }
  | expr TIMES  expr { Binop($1, Mult,  $3)              }
  | expr DIVIDE expr { Binop($1, Div,   $3)              }
  | expr EQ     expr { Binop($1, Equal, $3)              }
  | expr LT     expr { Binop($1, Less,  $3)              }
  | expr GT     expr { Binop($1, Greater, $3)            }
  | expr AND    expr { Binop($1, And,   $3)              }
  | expr OR     expr { Binop($1, Or,    $3)              }
  | MINUS expr %prec NOT { Unop(Neg, $2)                 }
  | NOT expr         { Unop(Not, $2)                     }
  | ID ASSIGN expr   { Assign($1, $3, Noexpr)            }
  | ID LBRAC expr RBRAC ASSIGN expr { Assign($1, $3, $6) }
  | ID DOT ID        { Attr($1, $3, Noexpr, Noexpr)              } 
  | ID DOT ID LBRAC expr RBRAC { Attr($1, $3, $5, Noexpr)        } 
  | ID DOT ID LBRAC expr COMMA expr RBRAC { Attr($1, $3, $5, $7)        } 
  | ID DOT ID ASSIGN expr { NodeAttr(Id($1), $3, $5) }  
  | ID LBRAC expr RBRAC { Access($1, $3)                 }
  | LPAREN expr RPAREN { $2                              }
  | ID LPAREN args_opt RPAREN { Call($1, $3)             }
  | ID COLON edge { EdgeList(Id($1), $3)}

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
