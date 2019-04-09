open Relation
open Value 
open Env

module R =
struct

  (* Création des opérations de manipulation des relations à valeur dans Value *)
  include Relation.Make(Value)

  (* Fonctions d'agrégation (à compléter...) *)
  let sum dist =
    fun a r -> fold dist (fun acc v -> match acc with None -> Some v | Some v' -> Some (Value.add v' v)) None a r

end






























(*-------------------------------  eval_query: (R.relation * R.attribute env) env → query → R.relation ------------------------------------*)



(*--------------------------------------------------  type 'a env = (string * 'a) list ----------------------------------------------------*)


(*Environnement d'attribut--------------------------  R.attribute env = (string * R.attribut) list ----------------------------------------*)
(*Environnement de relation-------------- (R.relation * R.attribute env) env = (string * (R.relation * R.attribute env) ) list ------------*)




(* (string * attribute) list * relation *)
let clientAtt,client = R.from_file "client.csv" '|'
let commandeAtt,commande = R.from_file "commande.csv" '|'
let vinAtt,vin = R.from_file "vin.csv" '|'
let viticulteurAtt,viticulteur = R.from_file "viticulteur.csv" '|'


let firstClientAttEnv = empty
let firstCommandeAttEnv = empty
let firstVinAttEnv = empty
let firstViticulteurAttEnv = empty



(*DEFINIR LES ENVIRONNEMENT D'ATTIBUTS*)
let rec ajoutEmptyEnv listAtt newEnvAtt = match listAtt with
                                      |[] -> newEnvAtt
                                      |t::q -> (match t with
                                                |(a,b) -> ajoutEmptyEnv q (add a b newEnvAtt)
                                                |_ -> failwith "erreur1"  
                                                )
                                      |d::[] -> (match d with
                                                |(a,b) -> (add a b newEnvAtt)
                                                |_ -> failwith "erreur2"  
                                                )
                                     
let clientAttEnv = ajoutEmptyEnv clientAtt firstClientAttEnv                                      
let commandeAttEnv = ajoutEmptyEnv commandeAtt firstCommandeAttEnv                                  
let vinAttEnv = ajoutEmptyEnv vinAtt firstVinAttEnv
let viticulteurAttEnv = ajoutEmptyEnv viticulteurAtt firstViticulteurAttEnv



(*DEFINIR L'ENVIRONNEMENT DES RELATIONS*)
let r1 = empty
let r2 = add "client" (client, clientAttEnv) r1
let r3 = add "commande" (commande, commandeAttEnv) r2
let r4 = add "vin" (vin, vinAttEnv) r3
let relationEnv = add "viticulteur" (viticulteur, viticulteurAttEnv) r4























(*TEST*)





(*Obtenir l'environnment des attributs apès une eval_query*)
let haveAttenv r = match r with
        |(a,b) -> b
        |_ -> failwith "environnment d'attribut inexistant"


(*Obtenir la relation apès une eval_query*)
let haveRel r = match r with
        |(a,b) -> a
        |_ -> failwith "relation inexistante"



let _ =

  (* Ouverture un flot de caractère ; ici à partir de l'entrée standard *)  
  let source = Lexing.from_channel stdin in

  (* Boucle infinie interompue par une exception correspondant à la fin de fichier *)
  let rec f () =
    try
      (* Récupération d'une expression à partir de la source puis affichage de l'évaluation *)
      let q = Parser.ansyn Lexer.anlex source in
      let s = Ast.string_of_query q in 
      let r,r_att = Ast.eval_query relationEnv q in
      let _ = Ast.R.print '|' [] r in
      Printf.printf"\n"; flush stdout;
      Printf.printf"%s ->\n" s; flush stdout;
      Printf.printf"\n"; flush stdout;
      
      f ()
    with Lexer.Eof -> Printf.printf "Bye\n"
  in

  f ()
























(*
let _ =
  let vin_att, vin = R.from_file "vin.csv" '|' in
  let _, viticulteur = R.from_file "viticulteur.csv" '|' in
  let _, client = R.from_file "client.csv" '|' in
  let _, commande = R.from_file "commande.csv" '|' in 
  let r1 = R.selection (fun t -> match R.attribute (List.assoc "Région" vin_att) t with Some (VVChar "Bordeaux") -> true | _ -> false) vin in
  let r2 = R.selection (fun t -> match R.attribute 4 t with Some (VVChar "Alsace") -> true | _ -> false) vin in
  let r3 = R.projection [ DInt, R.attribute 0 ; DInt, R.attribute 2 ; DInt, R.attribute 4 ] commande in
  let r4 = R.projection [ DInt, R.attribute 3 ] vin in
  let r4' = R.distinct r4 in
  let r5 = R.union viticulteur client in
  let r5' = R.distinct r5 in
  let r6 = R.inter viticulteur client in
  let r7 = R.diff viticulteur client in
  let r8 = R.diff client viticulteur in
  let r9 = R.crossjoin client commande in
  let r10 = R.crossjoin viticulteur vin in
  let r11 = R.innerjoin (fun v c -> R.attribute 2 c = R.attribute 0 v && match R.attribute 4 c with Some (VInt q) when q >= 30 -> true | _ -> false) vin commande in
  let r12 = R.innerjoin (fun vit vin -> R.attribute 3 vit = R.attribute 4 vin) viticulteur vin in
  let r13 = R.innerjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r14 = R.leftouterjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r15 = R.fullouterjoin (fun c v -> R.attribute 3 c = R.attribute 4 v) client vin in
  let r16 = R.fullouterjoin (fun v c -> R.attribute 2 c = R.attribute 0 v) vin commande in
  let r17 = R.aggregate [ 0 ] [ DInt, R.sum false 9 ] r16 in
  let _ = R.print '|' [] r17 in
  ()
*)
