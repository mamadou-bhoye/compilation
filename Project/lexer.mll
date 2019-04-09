{
open Parser
exception Eof

}


(* Déclaration du dictionnaire (regexp -> terminal/token) *)

rule anlex = parse
  | [' ' '\t' '\n' '\r']                        { anlex lexbuf (* Oubli des espacements et passages à la ligne *) }
  | "SELECT"                                    { SELECT }                   
  | "*"                                         { ASTERISK }
  | "FROM"                                      { FROM }
  | "TRUE"                                      { TRUE }

  | "'"                                         { mot "" lexbuf} 
  | "WHERE"                                     { WHERE }
  | '='                                        { EQ }                      
  | "--"                                        { comment lexbuf} 
  

  | "\""                     { QQUOTE }
  | '.'                      { DOT }
  | '('                      { LPAR }
  | ')'                      { RPAR }
  | 'E'                      { EXPONENT }
  | 'e'                      { EXPONENT }
  | '+'                      { PLUS }
  | '-'                      { MINUS }
  | '/'                      { SLASH }
  | '''                      { QUOTE }
  | "||"                     { PPIPE }
  | "<>"                     { NEQ }
  | '<'                      { LT }
  | '>'                      { GT }
  | "<="                     { LE }
  | ">="                     { GE }
  


  | "ALL"                    { ALL } 
  | "AND"                    { AND }                   
  | "AS"                     { AS }                   
  | "BETWEEN"                { BETWEEN }                   
  | "BY"                     { BY }                   
  | "CROSS"                  { CROSS }                   
  | "DISTINCT"               { DISTINCT }                   
  | "FALSE"                  { FALSE }                   
  | "FOR"                    { FOR }                   
  | "FULL"                   { FULL }                   
  | "GROUP"                  { GROUP }                   
  | "HAVING"                 { HAVING }                   
  | "INNER"                  { INNER }                   
  | "IS"                     { IS }                   
  | "JOIN"                   { JOIN }                   
  | "LEFT"                   { LEFT }                   
  | "LOWER"                  { LOWER }                   
  | "NOT"                    { NOT }                   
  | "NULL"                   { NULL }                   
  | "ON"                     { ON }                   
  | "OR"                     { OR }                   
  | "OUTER"                  { OUTER }                   
  | "RIGHT"                  { RIGHT }                   
  | "SUBSTRING"              { SUBSTRING }                   
  | "UNKNOWN"                { UNKNOWN }                   
  | "UPPER"                  { UPPER }


  | ['a'-'z' 'A'-'Z' '0'-'9']+ as lxm           { STRINGSANSQUOTE(lxm) }
  | ";;"                                        { TERM }
  | eof                                         { raise Eof }
  | _  as lxm                                   { (* Pour tout autre caractère : message sur la sortie erreur + oubli *)
                                                     Printf.eprintf "Unknown character '%c': ignored\n" lxm; flush stderr;
                                                     anlex lexbuf
                                                }


and mot s = parse
  | "'"                     {STRINGAVECQUOTE(s)}
  | [^ '\n']  as c          {mot (Printf.sprintf "%s%c" s c) lexbuf} 
  


and comment = parse
  | "\n"                     {anlex lexbuf}
  |_                         {comment lexbuf}


