open Graph
open Instruction
open Expr

let rec find ls x =
  match ls with
    | [] -> None
    | (y,v)::rest ->
      if y = x then Some(v) else find rest x

let rec lettuce ls x = 
  match ls with
    | [] -> None
    | fst::rest ->
      if x = fst then Some(x) else lettuce rest x

let findknown ls x =
  match find ls x with
    | None -> failwith "Not found in findknown"
    | Some(v) -> v

(* This example was helpful in generating the graph code: http://ocamlgraph.lri.fr/sudoku.ml *)

(* A module for constructing graphs whose vertices are strings *)
module G = Imperative.Graph.Abstract(struct type t = string end)

(* A module for coloring G-graphs *)
module C = Coloring.Mark(G)

let color_graph (colors : int) (nodes : string list) (edges : (string * string) list) : ((string * int) list) option =
  let g = G.create () in
  let add_vertex s =
    let vertex = G.V.create s in
    G.add_vertex g vertex;
    (s, vertex) in
  let vertices = List.map add_vertex nodes in
  let add_edge (source, dest) =
    let v1, v2 = (findknown vertices source, findknown vertices dest) in
    G.add_edge g v1 v2 in
  List.iter add_edge edges;
  begin try
    C.coloring g colors;
    Some(List.map (fun v -> (v, G.Mark.get (findknown vertices v))) nodes)
  with
    | _ -> None
  end

(* unions string list *)
let rec union_actives (actives : string list) (to_add : string list) : (string list) =
  begin match to_add with
   | (first::rest) -> 
     begin match lettuce actives first with
      | Some(x) -> (union_actives actives rest)
      | None -> (union_actives (first::actives) rest)
     end
    | [] -> actives
  end

(* unions (string, string) lists *)
let rec union_edges (edges : (string * string) list) (to_add : (string * string) list) : ((string * string) list) =
  begin match to_add with
   | (first::rest) -> 
     begin match lettuce edges first with
      | Some(x) -> (union_edges edges rest)
      | None -> (union_edges (first::edges) rest)
     end
    | [] -> edges
  end

(* Return all the identifiers that are defined in this expression (e.g. appear
in let bindings) to use for building an environment *)
let rec getvars (ae : aexpr) : string list =
  match ae with
   | ALet(x, bind, body) -> (union_actives (getvars body) [x])
   | ACExpr(ce) ->
     match ce with
       | CIf(cond, thn, els) ->
         (union_actives (getvars thn) (getvars els))
       | _ -> []
  
let rec find_coloring (current_num_colors : int) (varlist : string list) (edgelist : (string * string) list) : ((string * int) list) =
  let colorings = (color_graph current_num_colors varlist edgelist) in
  match colorings with
    | None -> (find_coloring (current_num_colors + 1) varlist edgelist)
    | Some(g) -> g

let rec nth_element (l : 'a list) (n : int) (curr_ind : int) : 'a =
  match l with 
    | first::rest -> 
      if n = curr_ind then
       first
      else
       (nth_element rest n (curr_ind + 1))

    | [] -> failwith (Printf.sprintf "out of bounds2. nth %d curr_ind %d" n curr_ind)

let get_colors (registers : reg list) (varlist : string list) (edgelist : (string * string) list) : (location envt) =
  let edgelist = (List.filter (fun x -> let vertex1 = (fst x) in
                                        let vertex2 = (snd x) in
                                        let v1_in_varlist = (List.exists (fun y -> if y = vertex1 then true else false) varlist) in
                                        let v2_in_varlist = (List.exists (fun y -> if y = vertex2 then true else false) varlist) in
                                        if (v1_in_varlist && v2_in_varlist) then true else false) edgelist) in
  let num_regs = (List.length registers) in
  let colorings = (find_coloring 1 varlist edgelist) in
  (List.map (fun x ->
    let ind = (snd x) in
      if ind < num_regs then
        ((fst x), (LReg(nth_element registers ind 0)))
      else
        ((fst x), (LStack((ind - (num_regs))))))
   colorings)

let get_id (ie : immexpr) : string list =
  match ie with
   | ImmId(x) -> [x]
   | _ -> []

let rec get_id_str_a (ae : aexpr) : string list = 
  match ae with
   | ALet(x, bind, body) -> [x] @ (get_id_str_c bind) @ (get_id_str_a body)
   | ACExpr(ce) -> get_id_str_c ce

and get_id_str_c (ce : cexpr) : string list =
  match ce with
   | CPrim1(op, i) -> get_id i
   | CPrim2(op, l, r) -> (get_id l) @ (get_id r)
   | CApp(fcn, binds) -> (List.flatten (List.map get_id binds))
   | CIf(cond, thn, els) -> (get_id cond)@(get_id_str_a thn)@(get_id_str_a els)
   | CImmExpr(ie) -> get_id ie

let rec del_elt (l : 'a list) (elt : 'a): 'a list = (* note: deletes only the first instance (from left to right) *)
  match l with
   | first::rest -> if first = elt then rest else first::(del_elt rest elt)
   | [] -> []

let rec dep_graph_ae (ae : aexpr) (actives : string list) : (string list * (string * string) list) =
  match ae with
   | ALet(x, bind, body) ->
     let dep_graph_body = (dep_graph_ae body actives) in
     let no_x = (del_elt (fst dep_graph_body) x) in (* x *)
     let dep_graph_bind = (dep_graph_ce bind no_x) in (* x *)
     let new_actives = (union_actives (no_x) (fst dep_graph_bind)) in (*body_actives + bind_actives - current binding*)
     
     (* snd dep_graph_bind is empty unless the binding contained an if *)
     (new_actives, (List.map (fun conflict -> (x, conflict)) new_actives) @ (snd dep_graph_bind) @ (snd dep_graph_body)) 

   | ACExpr(ce) -> (dep_graph_ce ce actives)

and dep_graph_ce (ce : cexpr) (actives : string list) : (string list * (string * string) list) =
  match ce with
   | CPrim1(op, ie) -> ((union_actives actives (get_id ie)), [])
   | CPrim2(op, l, r) -> ((union_actives actives (get_id_str_c ce)), [])
   | CApp(_, binds) ->
     ((union_actives actives
      (List.flatten
       (List.map get_id 
        (List.filter (fun x -> if (get_id x) = [] then false else true) binds)))), [])
   | CIf(cond, thn, els) -> 
     let dep_graph_thn = (dep_graph_ae thn actives) in
     let dep_graph_els = (dep_graph_ae els actives) in
     (*union dep_graph of cond and both branches*)
     ((union_actives (union_actives (fst dep_graph_thn) (fst dep_graph_els)) (get_id cond)), (union_edges (snd dep_graph_thn) (snd dep_graph_els)))
   | CImmExpr(ie) -> ((union_actives actives (get_id ie)), [])

let dep_graph (ae : aexpr) : (string * string) list =
  (snd (dep_graph_ae ae []))

let colorful_env (ae : aexpr) : location envt =
  let deps = dep_graph ae in
  (* spare_regs is a list that contains the usable registers for your
  implementation; it is set by the NUMREGS option as well *)
  get_colors !spare_regs (getvars ae) deps
