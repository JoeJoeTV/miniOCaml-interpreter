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

let parse l : exp * token list =
  let rec exp l = match l with
    |IF::l -> pif l
    |LAM::VAR(x)::ARR::l -> begin
      let (e, l) = exp l in
      ((Lam(x, e)), l) end
    |LAM::LP::VAR(x)::COL::l -> begin
      let (ty, l) = parsetype l in
      match l with
        |RP::ARR::l -> begin
          let (e, l) = exp l in
          (Lamty(x, ty, e), l) end
        |_ -> failwith "Lam: Unexpected token (Expected ')' and '->')" end
    |LET::VAR(x)::EQ::l -> begin
      let (e1, l) = exp l in
      match l with
        |IN::l -> begin
          let (e2, l) = exp l in
          (Let(x, e1, e2), l) end
        |_ -> failwith "Let: Missing 'in'" end
    |LET::REC::VAR(f)::VAR(x)::EQ::l -> begin
      let (e1, l) = exp l in
      match l with
        |IN::l -> begin
          let (e2, l) = exp l in
          (Let(x, e1, e2), l) end
        |_ -> failwith "Letrec: Missing 'in'" end
    |LET::REC::VAR(f)::LP::VAR(x)::COL::l -> begin
      let (ty1, l) = parsetype l in
      match l with
        |RP::COL::l -> begin
          let (ty2, l) = parsetype l in
            match l with
              |EQ::l -> begin
                let (e1, l) = exp l in
                match l with
                  |IN::l -> begin
                    let (e2, l) = exp l in
                    (Letrecty(f, x, ty1, ty2, e1, e2), l) end
                  |_ -> failwith "Missing 'in'" end
              |_ -> failwith "Missing '='" end
        |_ -> failwith "Letrecty: Unexpected token (Expected ')' and ':')" end
    |l -> cexp l

  and pif l =
    let (e1, l) = exp l in
    match l with
      |THEN::l -> begin
        let (e2, l) = exp l in
        match l with
          |ELSE::l -> begin
            let (e3, l) = exp l in
            (If(e1, e2, e3), l) end
          |_ -> failwith "Missing 'else'" end
      |_ -> failwith "Missing 'then'"

  and cexp l = 
    let (e, l) = sexp l
    in cexp' e l

  and cexp' e l = match l with
    |LEQ::l -> begin
      let (e1, l) = sexp l
      in cexp' (Oapp(Leq, e, e1)) l end
    |_ -> (e, l)

  and sexp l = 
    let (e, l) = mexp l
    in sexp' e l

  and sexp' e l = match l with
    |ADD::l -> begin
      let (e1, l) = mexp l
      in sexp' (Oapp(Add, e, e1)) l end
    |SUB::l -> begin
      let (e1, l) = mexp l
      in sexp' (Oapp(Sub, e, e1)) l end
    |_ -> (e, l)

  and mexp l = 
    let (e, l) = aexp l
    in mexp' e l

  and mexp' e l = match l with
    |MUL::l -> begin
      let (e1, l) = mexp l
      in sexp' (Oapp(Mul, e, e1)) l end
    |_ -> (e, l)

  and aexp l = 
    let (e, l) = pexp l
    in aexp' e l

  and aexp' e l = match l with
    |VAR(v)::l -> begin
      let (e1, l) = pexp (VAR(v)::l)
      in aexp' (Fapp(e, e1)) l end
    |CON(c)::l -> begin
      let (e1, l) = pexp (CON(c)::l)
      in aexp' (Fapp(e, e1)) l end
    |LP::l -> begin
      let (e1, l) = pexp (LP::l)
      in aexp' (Fapp(e, e1)) l end
    |_ -> (e, l)
  
  and pexp l = match l with
    |CON(BCON(b))::l -> ((Con(Bcon(b))), l)
    |CON(ICON(n))::l -> ((Con(Icon(n))), l)
    |VAR(v)::l -> ((Var(v)), l)
    |LP::l -> begin
      let (e, l) = exp l
      in begin match l with
        |RP::l -> (e, l)
        |_ -> failwith "Missing parenthesis" end end
    |l -> failwith "Unexpected Token"

  in exp l;;