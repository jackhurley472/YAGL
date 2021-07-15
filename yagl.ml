(* 
Implementation for language

Top-level of the YAGL compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module 
   
Minor helper functions of the large preprocessing code for file reading were adapted from Stack Overflow.   
*)


type action = Ast | Compile  | Sast | LLVM_IR

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in 

  (* Preprocessing of file for imports *)
  let usage_msg = "usage: ./yagl.native [-a|-s|-l|-c] [file.ygl]" in 
  let file_name = ref "" in
  let () = Arg.parse speclist (fun filename -> file_name := filename) usage_msg in
  let read_file = 
  let lines = ref [] in
  let chan = open_in !file_name in
  try
    while true; do
      lines := input_line chan :: !lines
      done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines
  in 
  let file_lines = read_file in
  let contains_import l1 =
  try
    let len = String.length "import" in
    for i = 0 to String.length l1 - len do
      if String.sub l1 i len = "import" then raise Exit
    done;
    false
  with Exit -> true
  in
      
  let read_import line = 
     let imported_file = List.nth (String.split_on_char ' ' line) 1 in
     let read_whole_file =
         let ch = open_in imported_file in
         let s = really_input_string ch (in_channel_length ch) in
             close_in ch;
             s
     in read_whole_file
  in    
  let import_file prev_lines curr_line = if contains_import curr_line then (read_import curr_line)::prev_lines else curr_line::prev_lines in  
  let new_file_lines = (List.rev (List.fold_left import_file [] file_lines)) in 
  let rec print_new_file l oc = match l with
     [] -> ()
     | h::t -> ignore(Printf.fprintf oc "%s\n" h); print_new_file t oc 
  in
  let new_file_name = (List.nth (String.split_on_char '.' !file_name) 0) ^ "_preprocessed.ygl" in
  let new_file_channel = open_out new_file_name 
  in print_new_file new_file_lines new_file_channel; close_out new_file_channel;
  

  (* Execute with preprocessed file now and remove it at the end*)
  let channel = ref stdin in  channel := open_in new_file_name; 
  let lexbuf = Lexing.from_channel !channel in
  let ast = Yaglparse.program Scanner.token lexbuf in  
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
    | _ -> let sast = Semant.check ast in
    
    match !action with
      Ast     -> ()
    | Sast    -> print_string (Sast.string_of_sprogram sast) 
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate sast))
    | Compile -> let m = Codegen.translate sast in
	Llvm_analysis.assert_valid_module m;
	print_string (Llvm.string_of_llmodule m)
  ; 
  Sys.remove new_file_name
