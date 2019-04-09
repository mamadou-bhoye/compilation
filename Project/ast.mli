open Relation
open Value
open Env

 module R :
  sig
    type domain = Value.domain
    type value = Value.value
    type relation = Relation.Make(Value).relation
    type tuple = Relation.Make(Value).tuple
    type attribute = int
    val attribute : attribute -> tuple -> value option
    val domain : relation -> attribute -> domain
    val width : relation -> int
    val cardinal : relation -> int
    val distinct : relation -> relation
    val selection : (tuple -> bool) -> relation -> relation
    val projection :
      (domain * (tuple -> value option)) list -> relation -> relation
    val union : relation -> relation -> relation
    val inter : relation -> relation -> relation
    val diff : relation -> relation -> relation
    val crossjoin : relation -> relation -> relation
    val innerjoin :
      (tuple -> tuple -> bool) -> relation -> relation -> relation
    val leftouterjoin :
      (tuple -> tuple -> bool) -> relation -> relation -> relation
    val fullouterjoin :
      (tuple -> tuple -> bool) -> relation -> relation -> relation
    val fold :
      bool -> ('a -> value -> 'a) -> 'a -> attribute -> relation -> 'a
    val aggregate :
      attribute list ->
      (domain * (relation -> value option)) list -> relation -> relation
    val from_file : string -> char -> (string * attribute) list * relation
    val to_file :
      string -> char -> (attribute * string) list -> relation -> unit
    val print : char -> (attribute * string) list -> relation -> unit
    val sum : bool -> attribute -> relation -> value option
  end
type source = Source of string
type projection = ProjectionAsterisk
type condition = Condition1 of string * string
type query =
    FirstTypeQuery of projection * source
  | SecondTypeQuery of projection * source * condition
val asterisk : projection 
val source : string -> source
val firstTypeQuery : projection -> source -> query 
val condition1 : string -> string -> condition 
val secondTypeQuery : projection -> source -> condition -> query 
val string_of_source : source -> string 
val string_of_projection : projection -> string 
val string_of_condition : condition -> string 
val string_of_query : query -> string 

val findAttInEnv : string -> 'a Env.env -> 'a 
val selectionEgaliteString :
  string -> 'a -> R.attribute Env.env -> R.relation -> R.relation 
val envEmpty : 'a Env.env
val eval_query :
  (R.relation * R.attribute Env.env) Env.env ->
  query -> R.relation * R.attribute Env.env 
