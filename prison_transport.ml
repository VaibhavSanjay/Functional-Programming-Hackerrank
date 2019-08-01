(*Given a list, it returns the minimum required cost*)
let rec get_individual_cost (prisoners : int) : int =
  let p = (float) prisoners in
  int_of_float @@ ceil (sqrt(p));;

(*Given a list of lists of ints, will give us the final cost*)
let rec get_final_cost (l: int array) (index: int) (cost: int): int =
  if index >= (Array.length l) then cost
  else get_final_cost l (index+1) (cost + (get_individual_cost (Array.get l index)));;

let rec find (p : int array) (x : int) : int =
      if p.(x) = x
      then x
      else
        let y = find p (p.(x)) in
        p.(x) <- y;
        y       
let union ([x;y] : int list) (p : int array) : int array =
  p.(find p (y-1)) <- p.(find p (x-1));
  p

let rec collect_input (m : int) : (int list list) =
  match m with
  | 0 -> []
  | n -> 
  let group = List.map int_of_string @@ String.split_on_char ' ' (read_line ()) in
  group :: collect_input (m-1) 

let getp (n : int) : int array =
  let p = Array.make (n) 0 in
  for i=0 to (n-1) do
      p.(i) <- i
  done;
  p

let rec do_unions (p : int array) (pairs: int list list) : int array =
    match pairs with
    | [] -> p
    | h::t -> do_unions (union h p) t

let get_amounts (p : int array) : int array =
    let ret = Array.make (Array.length p) 0 in
    for i=0 to ((Array.length p) - 1) do
        let index = find p i in
        ret.(index) <- (ret.(index) + 1)
    done;
    ret


let () : unit =  
  let n = int_of_string @@ read_line () in 
  let m = int_of_string @@ read_line () in 
  let p = getp n in
  let pairs = collect_input m in
  let newp = do_unions p pairs in
  let chains = (get_amounts newp) in
  print_int (get_final_cost chains 0 0)
