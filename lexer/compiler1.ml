(* 文字列を以下のトークン列に分解して表示するプログラム：遷移規則をテーブル化
   EQ:  =    LEQ: <=   LT: <   PLUS: +  IF: if THEN: then  
   INT: 0 |([1-9][0-9]* )   ID: [a-z]([a-z]|[0-9])*
*)
(* トークンの定義 *)
type token = INVALID | EQ | LEQ | LT | PLUS | IF | THEN | INT | ID

(* 入力文字列を格納しておく場所 *)
let input_buffer = ref ""  

(* 現在読んでいるlexeme(語彙素）の先頭位置 *)
let pos_start = ref 0 (* start of the current lexeme *)

(* これから読む文字の位置 *)
let pos_current = ref 0 (* current position *)

(* 現在読んでいるlexeme中で最後に認識されたトークン *)
let last_token = ref INVALID (* last valid token *)

(* 現在読んでいるlexeme中で最後に認識されたトークンの次の文字の位置 *)
let last_pos = ref 0 (* the end of the last valid lexeme *)

(* エラー出力 *)
let report_error pos =
  print_string ("invalid token at position"^(string_of_int pos)^"\n")

(* 最後に認識されたトークンに相当する文字列（lexeme） *)
let get_lexeme() =
  let len = !last_pos - !pos_start in String.sub !input_buffer !pos_start len

(* トークンを表示 *)
let output_token token =
  match token with
   EQ -> print_string "EQ "
 | LEQ -> print_string "LEQ "
 | LT -> print_string "LT "
 | PLUS -> print_string "PLUS "
 | IF -> print_string "IF "
 | THEN -> print_string "THEN "
 | INT -> (print_string ("INT("^(get_lexeme())^") "))
 | ID -> print_string ("ID("^(get_lexeme())^") ")
 | INVALID -> assert false

(* 一文字読みこむ *)
type state = int
let rec readc (st: state) =
  let c = !input_buffer.[!pos_current] in
   (pos_current:= !pos_current+1; 
    if st=0 && c=' ' then (* 空白の処理は状態 0 のみ特別扱い *)
        (pos_start := !pos_start+1; readc st)
    else c)

(* マッチしたトークンとその場所を保存 *)
let save token = (last_token := token; last_pos := !pos_current)

(* オートマトンの遷移テーブル *)
let trtab: (char -> state * token) array =
  [| (fun c -> 
       match c with
         '=' -> (1, EQ) | '<' -> (2, LT) | '+' -> (1, PLUS)
       | 'i' -> (4, ID) | 't' -> (5, ID) | '0' -> (1, INT)
       | c -> if '1'<=c && c<='9' then (3, INT)
            else if 'a'<= c && c<='z' then (8, ID)
            else if c='\000' then (-1, INVALID)
            else (-2, INVALID));
     (fun c -> (-2, INVALID));  (* st = 1: end of token *)
     (fun c ->  (* st = 2 (q_lt) *)
        if c='=' then (1, LEQ) else (-2, INVALID));
     (fun c ->  (* st = 3: q_num *)
        if '0'<=c && c<='9' then (3, INT) else (-2, INVALID));
     (fun c ->  (* st = 4: q_i *)
        if c='f' then (8, IF) else
        if ('a'<= c && c<='z')||('0'<=c && c<='9') then (8, ID)
        else (-2, INVALID));
     (fun c ->  (* st = 5: q_t *)
        if c='h' then (6, ID) 
        else if ('a'<= c && c<='z')||('0'<=c && c<='9') then (8, ID)
        else (-2, INVALID));
     (fun c ->  (* st = 6: q_th *)
        if c='e' then (7, ID) 
        else if ('a'<= c && c<='z')||('0'<=c && c<='9') then (8, ID)
        else (-2, INVALID));
     (fun c -> (* st = 7: q_the *)
        if c='n' then (8, THEN) 
        else if ('a'<= c && c<='z')||('0'<=c && c<='9') then (8, ID)
        else (-2, INVALID));
     (fun c -> (* st=8; q_sym *)
           if ('a'<= c && c<='z')||('0'<=c && c<='9') then (8, ID)
           else (-2, INVALID))
 |]

(* 遷移表を参照する関数 *)
let lookup_tab (st: state) (c: char): state * token = (trtab.(st)) c

(* メイン *)
let rec main (input: string) =
   input_buffer := (input^"\000");
   pos_start := 0; pos_current := 0; last_token := INVALID;
   q 0

and q st =
  let c = readc st in
  let (st',token) = lookup_tab st c in
    if st'< -1 (* undefined transition *) then next()
    else if st'= -1 (* termination of input string *) then ()
    else (if token!=INVALID then save token else(); q st')

and next() =
     if !last_token=INVALID then report_error(!pos_current)
     else
       (output_token !last_token; pos_start := !last_pos;
        pos_current := !pos_start; last_token := INVALID;
        q 0)
   