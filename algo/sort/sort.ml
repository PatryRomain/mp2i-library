(* This file contains searching algorithms *)

(** [swap t i j] exchanges [t.(i)] and [t.(j)] *)
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;

(** [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then swap t j (j + 1)
      done
  done;;
  
(** [dicho t] trouve un élément d'une table en complexité (log(n)) **)
let dicho t e =
    let rec fonction_auxil debut fin t e = 
        let milieu = (debut + fin)/2 in
        if t.(milieu) > e 
            then fonction_auxil debut (milieu-1) t e 
        else 
            if t.(milieu) = e then milieu
            else fonction_auxil (milieu+1) fin t e
    in
    fonction_auxil 0 (Array.length t -1) t e