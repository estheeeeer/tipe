
type sommet = { id:int; equipe:int; mutable x:float; mutable y:float }
type arete = { s1:sommet; s2:sommet; mutable dist:float }
type graph = { nb_s:int; matrix_adj:float array array ; mutable sommets:sommet list}

type joueur = { sommet:sommet; acuite_visuelle:int; mutable potentiel_attaque:float; mutable potentiel_soin:float; mutable score:int; mutable actif:bool; mutable distance_deplacement_max:float; mutable distance_interaction_max:float; equipe:equipe }
and equipe = { number:int; seuil:int; nb_joueurs:int; mutable nb_joueurs_actifs:int; mutable joueurs_actifs:joueur list; spawn_zone:(float*float*float*float)}
type jeu = { equipeA:equipe; equipeB:equipe; graphe:graph ; tbl_joueurs:(int,joueur) Hashtbl.t}


let x1_init_min = 0.0
let y1_init_min = 100.0
let x1_init_max = 100.0
let y1_init_max = 100.0

let x2_init_min = 300.0
let y2_init_min = 100.0
let x2_init_max = 400.0
let y2_init_max = 100.0

let dist_max = 500.0

let calcul_dist (s1:sommet) (s2:sommet) : float = 
  (s1.x -. s2.x)/.(s1.y -. s2.y)

let rd_fl_between (a:float) (b:float) =
  (Random.float b +. a)

let init_sommet ?x_init:(x_init=(-1.)) ?y_init:(y_init=(-1.)) (id_init:int) (equipe_id:int) ((x_init_min,x_init_max,y_init_min,y_init_max):(float*float*float*float)) : sommet =
  let x_init_ref = ref x_init in 
  let y_init_ref = ref y_init in  
  if x_init=(-1.) then 
    x_init_ref := rd_fl_between x_init_min x_init_max;
  if y_init=(-1.) then 
    y_init_ref := rd_fl_between y_init_min y_init_max;
  { id=id_init; equipe=equipe_id; x=(!x_init_ref); y=(!y_init_ref) }

let init_arete (sommet1:sommet) (sommet2:sommet) : arete =
  { s1 = sommet1; s2 = sommet2; dist = calcul_dist sommet1 sommet2 }

let init_graph (nb_sommets:int) : graph =
  { nb_s = nb_sommets; matrix_adj = Array.make_matrix nb_sommets nb_sommets (-1.) ; sommets = []} 

let update_dists_sommet (s:sommet) (g:graph) =
  List.iter ( fun s1 -> g.matrix_adj.(s.id).(s1.id) <- (calcul_dist s1 s) ) (g.sommets)

let update_matrix_adj (g:graph) =
  List.iter (fun s -> update_dists_sommet s g) (g.sommets)

let new_sommet ?(x_init=(-1.)) ?(y_init=(-1.)) (id_init:int) (equipe_id:int) (spawn_zone_init:(float*float*float*float)) (g:graph) : sommet  =
  let s = init_sommet id_init equipe_id spawn_zone_init in
  update_dists_sommet s g;
  g.sommets <- s::g.sommets;
  s

let voisins ?(distance_max=dist_max) (s:sommet) (g:graph) : sommet list =
  List.filter ( fun s1 -> if (s.id <> s1.id) then (calcul_dist s s1) <= dist_max else false) g.sommets
