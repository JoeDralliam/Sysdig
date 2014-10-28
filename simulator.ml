open Netlist_ast



type program_state =
    {
      vars: value Env.t ;
      memories: value array Env.t
    }
		       
		
let evaluate_arg vars = function
  | Avar id -> Env.find id vars
  | Aconst v -> v

let operation_of_binop = function
  | Xor -> (fun a b -> (a || b) && not (a && b))
  | Or  -> (||)
  | And -> (&&)
  | Nand -> (fun a b -> not (a && b))

let evaluate_binop op v1 v2 =
  match (v1, v2) with
  | (VBit a, VBit b) -> VBit (op a b)
  | (VBit _, VBitArray _)
  | (VBitArray _, VBit _)
  | (VBitArray _, VBitArray _) ->
     assert false

let evaluate_mux a b c =
  match a with
  | VBit true -> b
  | VBit false -> c
  | _ -> assert false

let evaluate_not = function
  | VBit a -> VBit (not a)
  | _ -> assert false

let evaluate_concat v1 v2 =
  VBitArray (
      match (v1, v2) with
      | (VBit a, VBit b) -> [| a ; b |]
      | (VBitArray a, VBit b) ->
	 let la = Array.length a in
	 let res = Array.make (la + 1) false in
	 Array.blit a 0 res 0 la ;
	 res.(la) <- b ;
	 res
      | (VBit a, VBitArray b) ->
	 let lb = Array.length b in
	 let res = Array.make (lb + 1) false in
	 res.(0) <- a ;
	 Array.blit b 0 res 1 lb ;
	 res
      | (VBitArray a, VBitArray b) ->
	 let la = Array.length a in
	 let lb = Array.length b in
	 let res = Array.make (la + lb) false in
	 Array.blit a 0 res 0 la ;
	 Array.blit b 0 res la lb ;
	 res
    )

let evaluate_slice i1 i2 = function
  | VBitArray a ->
     VBitArray (Array.sub a i1 (i2 - i1 + 1))
  | VBit b ->
     assert (i1 = 0 && i2 = 0) ;
     VBit b
	    
let evaluate_select i = function
  | VBitArray a ->
     VBit a.(i)
  | VBit b ->
     assert (i = 0) ;
     VBit b

let int_of_bits = function
  | VBit b -> if b then 1 else 0
  | VBitArray a ->
     Array.fold_left
       (fun s b -> (s * 2) + if b then 1 else 0)
       0 a

let bool_of_bit = function
  | VBit b -> b
  | VBitArray _ -> assert false

let evaluate_expr ident old_vars vars memories expr = 
  let evaluate_arg = evaluate_arg vars in
  match expr with
  | Earg arg -> evaluate_arg arg
  | Ereg ident ->
     begin
       try Env.find ident old_vars
       with Not_found -> VBit false
     end
  | Enot arg -> evaluate_not (evaluate_arg arg)
  | Ebinop (op, a, b) ->
     evaluate_binop (operation_of_binop op) (evaluate_arg a) (evaluate_arg b)
  | Emux (a, b, c) ->
     evaluate_mux (evaluate_arg a) (evaluate_arg b) (evaluate_arg c)
  | Erom (addr_sz, word_sz, raddr) ->
     let rom = Env.find ident memories in
     let addr = int_of_bits (evaluate_arg raddr) in
     rom.(addr)
  | Eram (addr_sz, word_sz, raddr, wenable, waddr, data) ->
       let ram = Env.find ident memories in
       let addr = int_of_bits (evaluate_arg raddr) in 
       ram.(addr) 
  | Econcat (a, b) ->
     evaluate_concat (evaluate_arg a) (evaluate_arg b)
  | Eslice (i1, i2, a) ->
     evaluate_slice i1 i2 (evaluate_arg a)
  | Eselect (i, a) ->
     evaluate_select i (evaluate_arg a)

let update_expr ident vars memories expr =
  let evaluate_arg = evaluate_arg vars in
  match expr with
  | Eram (addr_sz, word_sz, _, wenable, waddr, data) ->
     let ram = Env.find ident memories in
     let enable = bool_of_bit (evaluate_arg wenable) in
     if enable
     then begin
	 let addr = int_of_bits (evaluate_arg waddr) in
	 ram.(addr) <- evaluate_arg data 
       end
  | _ -> ()
	    
      
let step program state inputs = 
    let vars =
      List.fold_left 
	(fun vars (id, expr) ->
	 Env.add id (evaluate_expr id state.vars vars state.memories expr) vars
	) inputs program.p_eqs
    in

    List.iter
	 (fun (id, expr) -> update_expr id vars state.memories expr)
	 program.p_eqs ;
    
    { vars ; memories = state.memories }


	
