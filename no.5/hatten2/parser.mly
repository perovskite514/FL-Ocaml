%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL 
%token <string> ID
%token LET IN				  
%token PLUS TIMES MINUS DIV
%token AND OR NOT
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR 
%token SEMISEMI

%start toplevel 
%type <Syntax.command> toplevel
%% 

toplevel:
  | expr SEMISEMI { CExp $1 }
  | LET var EQ expr SEMISEMI { CDecl ($2,$4) }
  | LET var EQ expr letexpr { CLet ($2,$4,$5) }
;

letexpr:
  | LET var EQ expr letexpr { CLet ($2,$4,$5) }
  | LET var EQ expr SEMISEMI { CDecl ($2, $4) }
;

expr:
  | LET var EQ expr IN expr     { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | a_expr                      { $1 } 
;

a_expr:
  | a_expr OR b_expr           { BOr($1,$3) }
  | b_expr                      { $1 }
;

b_expr:
  | b_expr AND c_expr            { BAnd($1,$3) }
  | c_expr                      { $1 }
;

c_expr:
  | NOT c_expr                  { BNot($2) }
  | d_expr                      { $1 }
;

d_expr:
  | d_expr EQ d_expr    { EEq($1,$3) }
  | d_expr LT d_expr    { ELt($1,$3) }
  | arith_expr                      { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr { EMul($1,$3) }
  | factor_expr DIV atomic_expr   { EDiv($1,$3) }
  | atomic_expr                   { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;
 
var:
  | ID { $1 }
;
