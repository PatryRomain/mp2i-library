(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;
(** [inver l] retourne l'inverse de [l] **)
let rec inver l = 
    let rec f l k = match l with
        | [] -> k
        | e::q -> f q (e::k)
    in
    f l []
;;
(** [rech k l] retourne si un élément k est présent ou non dans une liste l **)
let rec rech k l = match l with
    |[] -> false
    |e::q -> if e = k then true else rech k q
;;
(** [l_croi l] retourne si une liste est croisante ou non **)
let rec l_croi l = match l with
    | e1::e2::q -> if e1 > e2 then false else l_croi (e2::q)
    | _ -> true
