(* Ocamllex scanner for YAGL *)

{ open Yaglparse }

let digit = ['0' - '9']
let digits = digit+
let ascii = [' ' - '~']
let escapeChars = ('\\' ['b' 't' 'r' 'n'])

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| "["      { LBRAC }
| "]"      { RBRAC }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| '<'      { LT }
| ">"      { GT }
| "&&"     { AND }
| "||"     { OR }
| "|"      { BAR }
| "!"      { NOT }
| ":"      { COLON }
| "."      { DOT }
| "->"     { ARROW }
| "<-"     { REVARROW }
| "<->"    { BIARROW }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "char"   { CHAR }
| "float"  { FLOAT }
| "Graph"  { GRAPH }
| "String" { STRING }
| "Node"   { NODE }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| "'" escapeChars "'"                                 as lxm { match lxm.[2] with 
                                                                 'b' -> CHRLIT('\b')
                                                                |'t' -> CHRLIT('\t')
                                                                |'r' -> CHRLIT('\r')
                                                                |'n' -> CHRLIT('\n')
                                                                | _ -> raise (Failure "Internal error. Case should never be reached.")
                                                             } 
| "'" ascii "'"                                       as lxm { CHRLIT(String.get lxm 1) } 
| '"' (( ascii # '"' )* escapeChars*)+ '"'            as lxm { STRLIT(String.sub lxm 1 ((String.length lxm )-2) ) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
