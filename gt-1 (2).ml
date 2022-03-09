(**
Jonathan Carbonneau
I pledge my honor that I haie abided by the Steiens Honor System.
 *)

type 'a gt = Node of 'a*('a gt) list

let t : int gt =
   Node (33,
  [Node (12 ,[]);
  Node (77,
  [Node (37,
  [Node (14, [])]);
  Node (48, []);
  Node (103, [])])
  ])
let mk_leaf (n: 'a) : 'a gt =
    Node (n, [])
  
let rec height t =
     match t with
     |Node(i,kids) -> 1 + (List.fold_right max (List.map height kids) 0)

let rec size t = 
    match t with
    |Node(i,kids) -> 1 + (List.fold_right (fun a b -> a + b)  (List.map size kids) 0);;
     
let rec paths_to_leaves t =
      match t with
      | Node(_,[])-> [[]]
      | Node(_,_::kids) -> List.flatten (List.mapi (fun i child -> List.map (fun a -> i::a) (paths_to_leaves child)) kids)
      
let rec is_leaf_perfect t =
      match t with
      | Node(n, kids) -> List.fold_left (fun a b -> a && is_leaf_perfect b) false kids;;
      
let rec preorder t =
      match t with
      | Node (i, []) -> [i]
      | Node (i, kids) -> i::List.flatten ((List.map preorder kids))
      
let rec mirror t =
      match t with
      | Node(i, []) -> Node (i, [])
      | Node(i, kids) -> Node (i, List.map mirror (List.rev kids))
      
let rec mapt a (Node(b,kids)) = 
        Node (a b, List.map (mapt a) kids);;

let rec foldt : ('a -> 'b list -> 'b) -> 'a gt -> 'b = fun f t ->       
  match t with
  |Node(a, l) -> (f a (List.map (foldt f) l));;

let mirror' t =
     foldt (fun i rs-> Node(i, List.rev rs)) t