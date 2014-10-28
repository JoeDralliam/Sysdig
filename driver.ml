open Netlist_ast
open Simulator

let value_of_string s =
  if String.length s > 1
  then VBitArray (Array.init (String.length s) (fun i -> s.[i] = '1'))
  else VBit (s.[0] = '1')
		     
let check_type arg ty =
  if ty <> Netlist_ast.typeof arg
  then begin
      Format.printf "Requested type was [%a] but got [%a]\n"
		    Netlist_printer.print_ty ty
		    Netlist_printer.print_ty (Netlist_ast.typeof arg) ;
      flush stdout ;
      raise (Invalid_argument "Invalid type")
    end


type variables =
    {
      mutable pos: int ;
      lines: (value Env.t) array
    }

let of_file types f =
  let chan = open_in f in
  let rec read_lines () =
    try
      let l = input_line chan in
      let buf = Scanf.Scanning.from_string l in
      let rec parse_vars inputs =
	try
	  let inputs =
	    Scanf.bscanf buf " %s %s@,"
			 (fun id value ->
			  if id = ""
			  then raise End_of_file
			  else begin
			      let inp = value_of_string value in
			      check_type inp (Env.find id types) ;
			      Env.add id inp inputs
			    end)
	  in
	  parse_vars inputs
	with End_of_file ->
	  inputs
      in
      (parse_vars Env.empty) :: (read_lines ())
    with End_of_file ->
      []
  in
  { pos = 0 ; lines = Array.of_list (read_lines ()) }
       
let next_line ls =
  let l = ls.lines.(ls.pos) in
  ls.pos <- (succ ls.pos) mod (Array.length ls.lines) ;
  l
		  
type variable_read_method =
  | VFromUser
  | VFromValue of variables

type memory_read_method =
  | MFromUser
  | MFromChannel of in_channel

    
			       
type options =
    {
      nsteps: int ;
      read_input_method : variable_read_method ;
      read_memory_method : memory_read_method ;
      interactive : bool ;
      silent : bool
    }
		     


let check_size arg sz =
  if match arg with
     | VBit b -> 1 <> sz
     | VBitArray a -> Array.length a <> sz
  then raise (Invalid_argument "Invalid size")
	     
	     

let read_memory r_and_r meth =
  let read_word word_sz buf =
    let w =
      Scanf.bscanf buf " %s"
		   (fun s ->
		    print_endline s ;
		    if s = ""
		    then raise End_of_file ;
		    value_of_string s)
    in
    check_size w word_sz ;
    w
  in
  
  let impl addr_sz word_sz a str =
    let buf = Scanf.Scanning.from_string str in
    begin
      try
	for i = 0 to (1 lsl addr_sz) - 1
	do
	  a.(i) <- read_word word_sz buf
	done;
      with End_of_file -> () ;
    end
  in 
  let ch = 
    match meth with
    | MFromUser ->
       begin 
	 Printf.printf "File for ROMs and RAMs ? " ;
	 flush stdout ;
	 Scanf.scanf "%s" open_in
       end
    | MFromChannel ch -> ch
  in
  
  let rec memories_str mems =
    try
      let l = input_line ch in
      let si = String.index l ' ' in
      let id = String.sub l 0 si in
      let mem_val = String.sub l (si + 1) (String.length l - si - 1) in
      memories_str (Env.add id mem_val mems)
    with End_of_file ->
      mems
  in

  let mems = memories_str Env.empty in
  List.fold_left (fun memories (ident, addr_sz, word_sz) ->
		  let bits = Array.make word_sz false in
		  let a = Array.make (1 lsl addr_sz) (VBitArray bits) in
		  begin
		    try
		      let str = Env.find ident mems in
		      impl addr_sz word_sz a str
		    with Not_found -> ()
		  end ;
		  Env.add ident a memories)
		 Env.empty r_and_r
  
  

		     
let read_input types ids = function
  | VFromUser ->
     let impl ty iid =
       Printf.printf "%s ? " iid ;
       flush stdout ;
       let inp =
	 Scanf.scanf " %s" value_of_string
       in
       check_type inp ty ;
       inp
     in

     List.fold_left
	    (fun inputs id ->
	     let ty = Env.find id types in
	     Env.add id (impl ty id) inputs
	    ) Env.empty ids

  | VFromValue lines ->
     next_line lines
	    
let print_out_variables outputs vars = 
  List.iter 
    (fun id ->
     Format.printf "%-6s = %a@." id Netlist_printer.print_value (Env.find id vars) ;
    ) outputs ;
  print_newline ()
		
let print_variables vars = 
  Env.iter 
    (fun id value ->
     Format.printf "%-6s = %a@." id Netlist_printer.print_value value ;
    ) vars ;
  print_newline ()
		
let print_memories memories =
  let print_memory ff mem =
    Array.iteri (fun i v -> Format.printf " @[[%d : %a]@]" i Netlist_printer.print_value v) mem
  in    
  Env.iter
    (fun id mem ->
     Format.printf "Mem of %-6s :%a@." id print_memory mem
    ) memories ;
  print_newline ()

let print_help () = 
  Printf.printf "%s\n%s\n%s\n%s\n%s\n%s\n"
		"s(tep) : Evaluate next step"
		"o(utput) : Print output variables"
		"m(emory) : Print memories"
		"v(ariable) : Print variables"
		"q(uit) : Quit"
		"h(elp) : Print this help" ;
  print_newline ()

let print_unknown () =
    Printf.printf "Unknown command ; type h or help for description of available commands\n" ;
    print_newline ()
		  
let user_wants_to_evaluate_next_step outputs state =
  let rec interaction () =
    Printf.printf ">> " ;
    flush stdout ;
    match String.lowercase (read_line ()) with
    | "s" | "step" -> true
    | "o" | "output" -> (print_out_variables outputs state.vars ; interaction ())
    | "m" | "memory" -> (print_memories state.memories ; interaction ())
    | "v" | "variable" -> (print_variables state.vars ; interaction ())
    | "q" | "quit" -> false			
    | "h" | "help" -> (print_help () ; interaction ())
    | _ -> (print_unknown () ; interaction ())
  in interaction ()

	    
let simulation p options =
  try
    let program = Scheduler.schedule p in
    let rec impl state count =
      if count <> 0
      then
	begin
	  let inputs =
	    if List.length program.p_inputs > 0
	    then read_input program.p_vars
			    program.p_inputs
			    options.read_input_method
	    else Env.empty
	  in
	  
	  
	  let state = Simulator.step program state inputs in
	  
	  if not options.silent
	  then begin
	      print_newline () ;
	      Printf.printf "**** Step outputs ****\n\n" ;
	      if List.length program.p_outputs > 0
	      then print_out_variables program.p_outputs state.vars
	    end ;
	  
	  if (not options.interactive)
	     || (user_wants_to_evaluate_next_step program.p_outputs state)
	  then impl state (if count > 0 then pred count else count)				     
	end 
    in
    let vars = Env.empty in
    let r_and_r = Scheduler.roms_and_rams program in
    let memories =
      if List.length r_and_r > 0
      then read_memory r_and_r options.read_memory_method
      else Env.empty
    in
    impl { vars ; memories } options.nsteps
  with Scheduler.Combinational_cycle ->
    Format.eprintf "Combinatorial cycle detected"

