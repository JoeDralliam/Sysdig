open Netlist_ast

exception Combinational_cycle

let arg_of_list id l =
  List.fold_left (fun res x -> match x with
    | Avar i -> (id,i) :: res
    | Aconst _ -> res
  ) [] l

let read_exp (id,exp) = match exp with
  | Earg (Avar i) 
  | Enot (Avar i)
  | Ebinop (_,Avar i,Aconst _)
  | Ebinop (_,Aconst _,Avar i)
  | Erom (_,_,Avar i)
  | Eram (_,_,Avar i,_,_,_)
  | Econcat (Avar i, Aconst _)
  | Econcat (Aconst _, Avar i)
  | Eslice (_,_,Avar i)
  | Eselect (_, Avar i)
      -> [(id,i)]

  | Ebinop (_,Avar i1,Avar i2)
  | Econcat ( Avar i1,Avar i2)
      -> [(id,i1) ; (id,i2)]
  
  | Emux (a1,a2,a3)  
    -> arg_of_list id [a1;a2;a3]

  | _ -> []



    
let graph_of_program p =
  let open Graph in
  let g = mk_graph () in
  List.iter (fun id ->
    add_node g id
  ) p.p_inputs ;

  List.iter (fun (id,_) ->
    add_node g id
  ) p.p_eqs ;


  List.iter (fun eq ->
    List.iter (fun (dep,but) ->
      add_edge g dep but
    ) (read_exp eq)
  ) p.p_eqs ;
  g


let schedule p =
  let g = graph_of_program p in
  try
    let l = Graph.topological g in
    { p with p_eqs = List.fold_left 
        (fun res id -> 
          try 
            (id,List.assoc id p.p_eqs) :: res
          with Not_found ->
            res) [] l }
  with Graph.Cycle ->
    raise Combinational_cycle


let registers p =
  List.fold_left 
    (fun acc (_,expr) ->
     match expr with
     | Ereg id -> id :: acc
     | _ -> acc
    ) [] p.p_eqs  

	  
let roms_and_rams p =
  List.fold_left
    (fun memories (id,expr) ->
     match expr with
     | Erom (addr_sz, word_sz, _) ->
	(id, addr_sz, word_sz) :: memories
     | Eram (addr_sz, word_sz, _, _, _, _) ->
	(id, addr_sz, word_sz) :: memories
     | _ -> memories
    ) [] p.p_eqs
