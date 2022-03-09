(*
Jonathan Carbonneau.
I pledge my honor that I have abided by the Stevens Honor System.
*)

let a = [1;2;3;4;5];;
let letter_e = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1];;
let sq = [[0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1]; [0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1]];;

let mirror : int -> int = fun x  ->
    match x with
	    0 -> 0 
	  | 1 -> 1
	  | 2 -> 4
    | 3 -> 5
    | 4 -> 2
    | 5 -> 3
    | _ -> 9 ;;

let mirror_image : int list -> int list = fun string_of_int ->
    List.map mirror string_of_int;;

let rotate_90 : int -> int = fun x ->
    match x with
	    0 -> 0 
	  | 1 -> 1
	  | 2 -> 3
    | 3 -> 4
    | 4 -> 5
    | 5 -> 2
    | _ -> 9 ;;

let rotate_90_letter : int list -> int list = fun letter ->
    List.map rotate_90 letter;;
    
let rotate_90_word : int list list -> int list list = fun word ->
    List.map rotate_90_letter word;;


let rec repeat : int -> 'a -> 'a list = fun n x ->
  match n with
  | 0 -> []
  | n -> x :: (repeat (n-1) x);;

let rec coverageHelper : int*int -> int list -> int -> (int*int) list = fun start stuff curr ->
  match start,stuff,curr with
    | _,[],_ -> []
    | (i,j),x::xs,1 ->
        (match x with
          0 -> coverageHelper (i,j) (x :: xs) 0
        | 2 -> coverageHelper (i,j+1) xs 1
        | 3 -> coverageHelper (i+1,j) xs 1
        | 4 -> coverageHelper (i,j-1) xs 1
        | 5 -> coverageHelper (i-1,j) xs 1
        | _ -> coverageHelper (i,j) xs 1
        )
    | (i,j),x::xs,0 ->
       (match x with
          0 -> (i,j) :: coverageHelper (i,j) xs 0
        | 1 -> (i,j) :: coverageHelper (i,j) xs 1
        | 2 -> (i,j+1) :: coverageHelper (i,j+1) xs 0
        | 3 -> (i+1,j) :: coverageHelper (i+1,j) xs 0
        | 4 -> (i,j-1) :: coverageHelper (i,j-1) xs 0
        | 5 -> (i-1,j) :: coverageHelper (i-1,j) xs 0
        | _ -> coverageHelper (i,j) xs 0
       )
    | _,_,_ -> [];;

let coverage : int*int -> int list -> (int*int) list = fun start stuff ->
    start::coverageHelper start stuff 1;;

let rec count: 'a list -> 'a -> int = fun p e ->
  match p with 
  | [] -> 0
  | x::xs ->
    if x = e
    then 1 + (count xs e)
    else 0;;

let rec remove : 'a list -> int -> 'a list = fun p n ->
    match p,n with
    | [],_ -> []
    | x::xs,0 -> x::xs
    | x::xs,n -> remove xs (n-1);;


let rec compress : int list -> (int*int) list = fun p ->
  match p with
  | [] -> []
  | x::xs -> 
    if x > -1 || x < 6
    then (x,(1 + count xs x)) :: compress (remove xs (count xs x))
    else compress xs;;

let getx (a,_) = a;;

let gety (_,a) = a;;

let rec uncompress : (int*int) list -> int list = fun p ->
  match p with
  | [] -> []
  | x::xs -> 
    if getx x > -1 && getx x < 6 && gety x > 0 
    then (repeat (gety x) (getx x)) @ (uncompress xs)
    else uncompress xs;;

let rec optimizeHelper : int list -> int -> int list = fun p curr ->
  match p with
  | [] -> []
  | x::xs -> 
    (match x with
      0 -> 
      if x != curr then x :: optimizeHelper xs 0
      else optimizeHelper xs 0
    | 1 -> 
      if x != curr then x :: optimizeHelper xs 1
      else optimizeHelper xs 1
    | _ -> x :: optimizeHelper xs curr
    )
    ;;

let optimize : int list -> int list = fun p ->
  optimizeHelper p 1;;

let pantograph : int -> int list -> int list = fun n letter->
  optimize (List.flatten (List.map (repeat n) letter));;
