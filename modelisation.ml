open Graph_team
type joueur = { sommet:sommet; acuite_visuelle:int; mutable potentiel_attaque:float; mutable potentiel_soin:float; mutable score:int; mutable actif:bool; mutable distance_deplacement_max:float; mutable distance_interaction_max:float; equipe:equipe }
and equipe = { number:int; seuil:int; nb_joueurs:int; mutable nb_joueurs_actifs:int; mutable joueurs_actifs:joueur list; spawn_zone:(float*float*float*float)}
type jeu = { equipeA:equipe; equipeB:equipe; graphe:graph ; tbl_joueurs:(int,joueur) Hashtbl.t}

let distance_interaction_max_default = 1.

let distance_deplacement_max_default = 1.
let last_id_joueur = ref 0
let score_init = 5

let portee_vue = 150.

let x_max = 400.0
let y_max = 200.0

exception ConflitEquipes of joueur*string*joueur
exception TropLoin of joueur*string*joueur
exception TentativeActionEchouee of joueur*string*joueur

(* fonctions equipe *)

let init_equipe (id_init:int) (seuil_init:int) (nb_joueurs_init:int) ((x_init_min,x_init_max, y_init_min, y_init_max):(float*float*float*float)) : equipe =
  let spawn_zone_init = (x_init_min,x_init_max,y_init_min,y_init_max) in 
  { number = id_init; seuil = seuil_init; nb_joueurs = nb_joueurs_init; nb_joueurs_actifs = nb_joueurs_init; joueurs_actifs = []; graphe = init_graph nb_joueurs_init id_init spawn_zone_init; spawn_zone = spawn_zone_init }


(* fonctions joueur *)

let id (j:joueur) : int =
  j.sommet.id

let init_joueur ?(distance_deplacement_max_init=distance_deplacement_max_default) ?(distance_interaction_max_init=distance_deplacement_max_default) (equipe_init:equipe) (acuite_visuelle_init:int) (potentiel_attaque_init:float) (potentiel_soin_init:float) (jeu:jeu) : joueur = 
  incr last_id_joueur; 
  { sommet = new_sommet (!last_id_joueur-1) equipe_init.id  equipe_init.spawn_zone jeu.graphe; acuite_visuelle = acuite_visuelle_init; potentiel_attaque = potentiel_attaque_init; potentiel_soin = potentiel_soin_init; score=score_init; actif=true; distance_deplacement_max = distance_deplacement_max_init; distance_interaction_max = distance_interaction_max_init; equipe=equipe_init }


let add_joueur (equipe_init:equipe) (acuite_visuelle_init:int) (potentiel_attaque_init:float) (potentiel_soin_init:float) (jeu:jeu) : joueur =
  let j = init_joueur equipe_init acuite_visuelle_init potentiel_attaque_init potentiel_soin_init jeu in 
  Hashtbl.add (jeu.tbl_joueurs) (id j) j;
  j
  
let meme_equipe (j1:joueur) (j2:joueur) : bool =
    j1.equipe.number = j2.equipe.number

let position (j:joueur) : (float*float) =
  (j.sommet.x,j.sommet.y)

let en_capacite_action (j1:joueur) (j2:joueur) : bool =
  let distance = calcul_dist j1.sommet j2.sommet in
  distance<=j.distance_interaction_max


let incremente_score (j:joueur) =
  j.score <- j.score-1

let decremente_score (j:joueur) =
  j.score <- j.score+1

let update_actif (j:joueur) =
  if (j.score < j.equipe.seuil) then j.actif <- false

let action_attaque (attaquant:joueur) (attaque:joueur)  =
  if not meme_equipe attaquant attaque then
    if en_capacite_action attaquant attaque then
      begin
        incremente_score attaquant;
        decremente_score attaque;
        update_actif attaque;
      end
    else raise TropLoin(attaquant,"attaque",attaque)
  else raise ConflitEquipes(attaquant,"attaque",attaque)

let action_soigne (soignant:joueur) (soigne:joueur) =
  if meme_equipe soignant soigne then
    if en_capacite_action soignant soigne then incremente_score soigne
    else raise TropLoin(soignant,"soigne",soigne)
  else 
    raise ConflitEquipes(soignant,"soigne",soigne)

let attaque (attaquant:joueur) (attaque:joueur) =
  let rd = Random.float 1. in
  if rd<=attaquant.potentiel_attaque then action_attaque
  else raise TentativeActionEchouee(attaquant,"attaque",attaque)

let soigne (soignant:joueur) (soigne:joueur) =
  let rd = Random.float 1. in
  if rd<=soignant.potentiel_soin then action_soigne
  else raise TentativeActionEchouee(soignant,"soigne",soigne)

let deplace_to (j:joueur) (x_depl:float) (y_depl:float) : (void) =
  j.sommet.x <- x_depl;
  j.sommet.y <- y_depl

let deplace_aleatoire (j:joueur) =
  let rec pos_al_in_grille () : (float*float) =  
    let angle = rd_fl_between 0. 360. in
    let x_al = (j.distance_deplacement_max)*(cos angle)+.(j.sommet.x) in
    let y_al = (j.distance_deplacement_max)*(sin angle)+.(j.sommet.y) in
    if (x_al<=x_max) && (y_al<=y_max) then (x_al,y_al)
    else pos_al_in_grille ()
  in 
  let (x_depl,y_depl) = pos_al_in_grille () in
  deplace_to j x_depl y_depl


let voisins_in_radius ?(sorted=true) (joueur:joueur) (r:float) (jeu:jeu) : (joueur list)*(joueur list) =
  let _is_allie (s:sommet) : bool =
    let other_joueur = Hashtbl.find (jeu.tbl_joueurs) s in
    (meme_equipe joueur other_joueur)
  in
  let all_voisins_in_radius = voisins ~distance_max:r joueur.sommet jeu.graphe in
  if sorted then
    let (allies,ennemis) = List.partition _is_allie (List.sort all_voisins_in_radius) in
    (allies,ennemis)
  else 
    let (allies,ennemis) = List.partition _is_allie all_voisins_in_radius in
    (allies,ennemis)

let ennemis_in_radius ?(sorted=true) (joueur:joueur) (r:float) (jeu:jeu) = 
  let (_,ennemis) = voisins_in_radius ~sorted:false joueur r jeu in
  if sorted then List.sort ennemis
  else ennemis

let allies_in_radius ?(sorted=true) (joueur:joueur) (r:float) (jeu:jeu) =
  let (allies,_) = voisins_in_radius ~sorted:false joueur  r jeu in 
  if sorted then List.sort allies
  else allies

(* fonctions jeu *)

let init_jeu (nb_joueurs_total:int) (spawn_zone_1:(float*float*float*float)) (spawn_zone_2:(float*float*float*float)) : jeu =
  let equipeA_init = init_equipe 0 0 (nb_joueurs_total/2) spawn_zone_1 in
  let equipeB_init = init_equipe 1 0 (nb_joueurs_total/2) spawn_zone_2 in
  let graph_init = init_graph nb_joueurs_total in
  { equipeA = equipeA_init; equipeB = equipeB_init; graphe = graph_init ; tbl_joueurs = Hashtbl.create nb_joueurs_total }


