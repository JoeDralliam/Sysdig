open Netlist_ast


       
let number_steps = ref (-1)
let interactive = ref false
let silent = ref false
let memfile = ref ""
let varfile = ref ""

    
let compile filename =
  try
    let p = Netlist.read_file filename in
    let options =
      Driver.({
	       nsteps = !number_steps ;
	       interactive = !interactive ;
	       silent = !silent ;
	       read_memory_method =
		 if !memfile = ""
		 then MFromUser
		 else MFromChannel (open_in !memfile) ;
	       read_input_method =
		 if !varfile = ""
		 then VFromUser
		 else VFromValue (of_file p.p_vars !varfile)
	     })
    in
    Driver.simulation p options
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
     ["-n", Arg.Set_int number_steps, "Number of steps to simulate" ;
      "-i", Arg.Set interactive, "Enable interactive mode" ;
      "-m", Arg.Set_string memfile, "File to load memories from" ;
      "-v", Arg.Set_string varfile, "File to load input variables from" ;
      "-s", Arg.Set silent, "Do not print output values"]
    compile
    "Simulator test"
;;

main ()
