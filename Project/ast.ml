open Value
open Relation
open Env

module R =
struct
 
  (* Création des opérations de manipulation des relations à valeur dans Data *)
  include Relation.Make(Value)
 
  (* Fonctions d'agrégation (à compléter...) *)
  let sum dist a r =
    fold dist (fun acc v -> match acc with None -> Some v | Some v' -> Some (Value.add v' v)) None a r
end






(* Syntaxe abstraite  -> type projection peut etre en plus...*)



type source = 
  | Source of string 


type projection =
  | ProjectionAsterisk 


(**)
type condition =
  | Condition1 of string * string  

(**)
type query = 
  | FirstTypeQuery of projection * source 
  | SecondTypeQuery of projection * source * condition
 






(* Constructeurs de querys  -->  utile dans le parser*)


let asterisk = ProjectionAsterisk 

let source nomRelation = Source nomRelation

let firstTypeQuery nomProjection nomSource = FirstTypeQuery (nomProjection,nomSource)

(**)
let condition1 motAgauche motADroite = Condition1 (motAgauche,motADroite)

let secondTypeQuery nomProjection nomSource condition = SecondTypeQuery (nomProjection,nomSource,condition) 



















(* Conversion en chaîne de caractères pour affichage *)



let rec string_of_source source = match source with
  |Source source ->  source


let rec string_of_projection projection = match projection with
  |ProjectionAsterisk ->  "*"


(**)
let rec string_of_condition condition = match condition with
  |Condition1 (motAgauche,motADroite) -> Printf.sprintf "%s = %s" motAgauche motADroite


(*val string_of_query : query -> string *)
let rec string_of_query query = match query with
	|FirstTypeQuery (projection,source) -> Printf.sprintf "SELECT %s FROM %s" (string_of_projection projection)(string_of_source source)
  |SecondTypeQuery (projection,source,condition) -> Printf.sprintf "SELECT %s FROM %s WHERE %s" (string_of_projection projection)(string_of_source source)(string_of_condition condition)














(* Evaluateur et Environnement *) 


(*Prend le nom d'un attribut, un attEn et cherche l'attribut correspondant*)
let findAttInEnv nomAttribut attEnv = match (find nomAttribut attEnv) with
                              |Some attribut -> attribut
                              |None -> failwith "aucun attributs"



let selectionEgaliteString motAgauche motADroite attEnv relation = R.selection (fun t -> match R.attribute (findAttInEnv motAgauche attEnv) t with 
                                              Some(VVChar motADroite) -> true  
                                              | _ -> false) relation 






(*  eval_query: (R.relation * R.attribute env) env → query → (R.relation * R.attribute env) *)
let envEmpty = empty

let rec eval_query env query = match query with
          |FirstTypeQuery(projection,source) -> (match source with
                                                  |Source source1 -> (match (find source1 env) with
                                                                        |Some relationEtAttEnv -> (match projection with
                                                                                                   |ProjectionAsterisk -> relationEtAttEnv
                                                                                                  )

                                                                        |_ -> failwith "relation inexistante"
                                                                     )
                                                )

          |SecondTypeQuery(projection,source,condition) -> (match source with
                                                            |Source source1 -> (match (find source1 env) with
                                                                                  |Some relationEtAttEnv -> (match projection with
                                                                                                              |ProjectionAsterisk -> (match relationEtAttEnv with
                                                                                                                                      |(relation,attEnv) -> (match condition with
                                                                                                                                                            |Condition1(motAgauche,motADroite) -> (selectionEgaliteString motAgauche motADroite attEnv relation, envEmpty) 
                                                                                                                                                            )
                                                                                                                                     )
                                                                                                             )
                                                                                  |_ -> failwith "relation inexistante"
                                                                               )
                                                            ) 

                          



      














