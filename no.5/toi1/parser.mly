%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL 
%token <string> ID 
%token PLUS 
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR 
%token SEMISEMI

%start toplevel 
%type <Syntax.command> toplevel
%% 

toplevel:
  | expr SEMISEMI { CExp $1 }
;

expr:
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | expr EQ expr    { EEq($1,$3) }
  | expr LT expr    { ELt($1,$3) }
  | expr PLUS factor_expr { EAdd($1,$3) }
  | factor_expr                 { $1 }
;

factor_expr: 
  | atomic_expr                 { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;
 

