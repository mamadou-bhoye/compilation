%{

open Ast

%}

/* Déclaration des terminaux */
%token FROM
%token SELECT
%token ASTERISK
%token TRUE

%token WHERE
%token EQ
%token <string> STRINGSANSQUOTE
%token <string> STRINGAVECQUOTE


%token TERM



 %token QQUOTE 
 %token DOT 
 %token LPAR 
 %token RPAR 
 %token EXPONENT 
 %token PLUS 
 %token MINUS 
 %token SLASH 
 %token QUOTE 
 %token PPIPE 
 %token NEQ 
 %token LT 
 %token GT 
 %token LE 
 %token GE 

        
%token ALL
%token AND                    
%token AS                   
%token BETWEEN                   
%token BY                    
%token CROSS                    
%token DISTINCT                    
%token FALSE                    
%token FOR                    
%token FULL                    
%token GROUP                    
%token HAVING                    
%token INNER                    
%token IS                    
%token JOIN                    
%token LEFT                    
%token LOWER                    
%token NOT                    
%token NULL                    
%token ON                    
%token OR                    
%token OUTER                    
%token RIGHT                    
%token SUBSTRING                    
%token UNKNOWN                    
%token UPPER                    
%token WHERE 




/* Précédences (priorité + associativité) des terminaux */


/* Déclaration du non-terminal axiome (ici, ansyn) et du type de son attribut */
%type <Ast.query>  ansyn
%start ansyn

%%

/* Déclaration de la grammaire avec les actions sémantiques */

ansyn:
  | TERM ansyn              { $2 }
  | query TERM               { $1 }  
;




query:
  | SELECT projection FROM source							  { firstTypeQuery $2 $4}
  | SELECT projection FROM source WHERE condition			  { secondTypeQuery $2 $4 $6}
  
;




projection:
  | ASTERISK							{asterisk}
;



source:
  | STRINGSANSQUOTE          			{ source $1 }
  
;

condition:
  | STRINGSANSQUOTE EQ STRINGAVECQUOTE		{condition1 $1 $3}






