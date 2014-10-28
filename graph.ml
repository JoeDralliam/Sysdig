exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let has_cycle g =
  let rec impl a_visiter =
    a_visiter.n_mark <- InProgress ;
    List.iter (fun ch ->
      match ch.n_mark with
        | Visited -> ()
        | InProgress -> raise Cycle
        | NotVisited -> impl ch 
    ) a_visiter.n_link_to ;
    a_visiter.n_mark <- Visited
  in
  try begin
    List.iter impl (find_roots g) ;
    clear_marks g ;
    false
  end
  with Cycle -> begin
    clear_marks g ;
    true
  end


let topological g =
  let rec impl res a_visiter =

    let res =
      List.fold_left (fun res ch ->
        match ch.n_mark with
          | Visited -> res
          | InProgress -> raise Cycle
          | NotVisited -> impl res ch 
      ) res a_visiter.n_link_to
    in
    a_visiter.n_mark <- Visited ;
    a_visiter.n_label :: res
  in
  let res = List.fold_left impl [] (find_roots g) in
  clear_marks g ;
  if List.length res < List.length g.g_nodes
  then raise Cycle
  else res
