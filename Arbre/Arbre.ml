(** Ce fichier contient la défnition d'un arbre binaire d'int et des fonctions associées *)

(** Défintion d'un arbre binaire d'entier **)
type arbre_bin_int =  Feuille of int | Branche of arbre_bin_int * arbre_bin_int 

(** la fonction [feuille_droite a] renvoie l'élément le plus à droite d'un arbre **)
let rec feuille_droite a = match a with
    | Feuille f -> f
    | Branche ( g, d) -> feuille_droite d

(** la fonction [del_feuille_droite a] renvoie l'arbre sans l'élément le plus à droite d'un arbre **)
let rec del_feuille_droite a = match a with
    | Feuille f -> failwith "Plus d'arbre"
    | Branche ( g, Feuille f) -> g
    | Branche ( g, d) -> Branche (g, del_feuille_droite d)

(** la fonction [feuille_gauche a] renvoie l'élément le plus à gauche d'un arbre **)
let rec feuille_gauche a = match a with
    | Feuille f -> f
    | Branche ( g, d) -> feuille_gauche g

(** la fonction [del_feuille_gauche a] renvoie l'arbre sans l'élément le plus à gauche d'un arbre **)
let rec del_feuille_gauche a = match a with
    | Feuille f -> failwith "Plus d'arbre"
    | Branche ( Feuille f, d) -> d
    | Branche ( g, d) -> Branche (del_feuille_gauche g, d)

(** la fonction [ajout_feuille f a] ajoute un élément f dans un arbre a trié de en conservant le tri de l'arbre**)
let rec ajout_feuille f a = match a with
    | Feuille a -> if a > f then Branche ( Feuille f, Feuille a) else Branche ( Feuille a, Feuille f)
    | Branche ( g, d) -> if f > feuille_droite g then Branche ( g, ajout_feuille f d) else Branche (ajout_feuille f g, d) 

(** la fonction [tri a] retourne un arbre dont les valeurs à gauche sont toujours plus grande que celles à droite **) 
let rec tri a = match a with
    | Feuille f -> a
    | Branche ( Feuille f1, Feuille f2) -> 
        if f1 > f2 
            then Branche ( Feuille f2, Feuille f1) 
        else a
    | Branche (Feuille f1, Branche (g,d)) | Branche (Branche (g,d), Feuille f1) -> 
        let tri_d = tri (Branche (g,d)) in
        if f1 > feuille_gauche tri_d 
            then ajout_feuille f1 tri_d
        else a
    | Branche ( g, d) ->
        let g_tri = tri g in
        let d_tri = tri d in
        let f = feuille_gauche d_tri in
        if feuille_droite g_tri > f
        then tri (Branche ( ajout_feuille f g_tri, del_feuille_gauche d_tri))  
(** Dans ce cas là, cela revient à appliquer un quicksort aux multiple petites branches que l'on obtiendra : on retire un élément mal placée d'une branche pour le rajouter à une autre **)
        else a 
    
(** la fonction [plus_lointaine_feuille a] retourne le plus grand nombre de branche à la suite dans un arbre a **) 
let plus_lointaine_feuille a =
    let rec aux a c = match a with
        | Feuille f -> c
        | Branche ( g , d ) -> 
            let prof_gauche = aux g (c+1) in
            let prof_droite = aux d (c+1) in
            if prof_droite > prof_gauche then prof_droite else prof_gauche
    in
    aux a 0

(** la fonction [equilibrage a] retourne un arbre pour lequelle toutes les feuilles apparaissent après le même nombre (+/- 1) de branche**) 
let rec equilibrage a = match a with
    | Feuille f -> a
    | Branche (g,d) -> 
        let p_gauche = plus_lointaine_feuille g in
        let p_droite = plus_lointaine_feuille d in
        if p_gauche-1 <= p_droite && p_droite <= p_gauche +1
        (** On vérifie que les deux branches ont leurs feuilles les plus éloignés à une même profondeur**)
            then a 
        else if p_gauche < p_droite 
            then begin
                let nouv_g = ajout_feuille (feuille_gauche d) g in
                let nouv_d = del_feuille_gauche d in
                (** on s'assure que chaque couple de Branche a approximativement le même nombre de branche après eux **)
                equilibrage (Branche (equilibrage nouv_g, equilibrage nouv_d));
                end
            else begin
                let nouv_d = ajout_feuille (feuille_droite g) d in
                let nouv_g = del_feuille_droite g in
                equilibrage (Branche (equilibrage nouv_g, equilibrage nouv_d));
                end
