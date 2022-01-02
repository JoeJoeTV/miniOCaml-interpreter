(*Types*)
type const = BCON of bool | ICON of int
type token = COL | ADD | SUB | MUL | LP | RP | EQ | LEQ | ARR
  | IF | THEN | ELSE | LAM | LET | IN | REC
  | CON of const | VAR of string | BOOL | INT

type ty  = Int | Bool | Arrow of ty * ty;;
type var = string;;
type con = Bcon of bool | Icon of int;;
type op  = Add | Sub | Mul | Leq;;
type exp = Var of var | Con of con
  | Oapp of op * exp * exp
  | Fapp of exp * exp
  | If of exp * exp * exp
  | Lam of var * exp
  | Lamty of var * ty * exp
  | Let of var * exp * exp
  | Letrec of var * var * exp * exp
  | Letrecty of var * var * ty * ty * exp * exp;;


let verify l tk = match l with
  |tk'::l -> tk' = tk
  |_ -> failwith "End of Token List";;

let tlist2str l =
  let rec token2str tk =
    match tk with
      |BOOL -> "BOOL"
      |INT -> "INT"
      |ARR -> "ARR"
      |LP -> "LP"
      |RP -> "RP"
      |_ -> "#"
            
  in let rec tlist2str l =
    match l with
      |tk::[] -> token2str tk
      |tk::l -> (token2str tk) ^ "; " ^ (tlist2str l)
      |[] -> ""
      
  in "[" ^ (tlist2str l) ^ "]";;


(*let parse l : exp * token list =
  let rec exp l : exp * token list = match l with
    |IF::l -> 
    |LAM::VAR(x)::ARR::l ->
    |LAM::LP::VAR(x)::COL::l ->
    |LET::VAR(x)::EQ::l -> 
    |LET::REC::VAR(f)::VAR(x)::EQ::l ->
    |LET::REC::VAR(f)::LP::VAR(x)::COL::l ->
    |l -> cexp l
  
  and ty l : exp * token list = let (l, t) = (pty l) 
    in ty' l t
  
  and ty' (l : exp * token list) (t : ty) = match l with
    |[] -> 
    |ARR::l -> let (l, t1) = ty l
      in let (l, t2) = ty' l t1
      in ()
  and cexp l : exp * token list =
  
  
  
    in exp l;;*)

let parsetype l : ty * (token list) =
  let rec ty l = 
    let (t, l) = pty l
    in ty' t l

  and ty' t l = match l with
    |ARR::l -> begin
      let (t1, l) = pty l
      in let (t2, l) = ty' t1 l
      in ((Arrow(t, t2)), l) end
    |_ -> (t, l)

  and pty l = match l with
    |BOOL::l -> (Bool, l)
    |INT::l -> (Int, l)
    |LP::l -> begin
      let (t, l) = ty l
      in begin match l with
        |RP::l -> (t, l)
        |_ -> failwith "Missing parenthesis" end end
    |l -> failwith ("Unexpected Token: " ^ tlist2str l)

  in ty l