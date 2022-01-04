(*
  Name: interpreter.ml
  Description: Interpreter for the MiniOCaml "language"
  Author: JoeJoeTV
  Extra Features:
    - Lexer supports Comments
    - Intepreter supports Pairs
*)

(* Types *)
type const = BCON of bool | ICON of int
type token = COM | COL | ADD | SUB | MUL | LP | RP | EQ | LEQ | ARR
  | IF | THEN | ELSE | LAM | LET | IN | REC
  | CON of const | VAR of string | BOOL | INT

type ty  = Int | Bool | Arrow of ty * ty | Pair of ty * ty
type var = string
type con = Bcon of bool | Icon of int
type op  = Add | Sub | Mul | Leq
type exp = Var of var | Con of con
  | Oapp of op * exp * exp
  | Fapp of exp * exp
  | If of exp * exp * exp
  | Lam of var * exp
  | Lamty of var * ty * exp
  | Let of var * exp * exp
  | Letrec of var * var * exp * exp
  | Letrecty of var * var * ty * ty * exp * exp
  | Pairexp of exp * exp
  | Letpair of var * var * exp * exp

type ('a,'b) env = ('a * 'b) list

type value = Bval of bool | Ival of int
  | Closure of var * exp * (var, value) env
  | Rclosure of var * var * exp * (var, value) env
  | Pair of value * value

(* Functions *)

(** General Helper Functions **)

let empty : ('a,'b) env = []
let update (env : ('a,'b) env) a b : ('a,'b) env = (a,b) :: env
let rec lookup (env : ('a,'b) env) a =  match env with
  | (a',b) :: env -> if a = a' then Some b else lookup env a
  | [] -> None

let ty2str ty =
  let rec ty2str ty = match ty with
    |Int -> "int"
    |Bool -> "bool"
    |Pair(ty1, ty2) -> (ty2str ty1) ^ " * " ^ (ty2str ty2)
    |Arrow(ty1, ty2) -> (ty2str ty1) ^ " -> " ^ (ty2str ty2)
  in ty2str ty

let fsttoken2str l =
  match l with
    |tk::l -> begin
      match tk with
        |COM -> ","
        |COL -> ":"
        |ADD -> "+"
        |SUB -> "-"
        |MUL -> "*"
        |LP -> "("
        |RP -> ")"
        |EQ -> "="
        |LEQ -> "<="
        |ARR -> "->"
        |IF -> "if"
        |THEN -> "then"
        |ELSE -> "else"
        |LAM -> "fun"
        |LET -> "let"
        |IN -> "in"
        |REC -> "rec"
        |BOOL -> "bool"
        |INT -> "int"
        |CON(BCON(b)) -> "<bool>"
        |CON(ICON(n)) -> "<int>" 
        |VAR(v) -> v end
    |[] -> "#"

let tlist2str l =            
  let rec tlist2str l =
    match l with
      |tk::[] -> fsttoken2str [tk]
      |tk::l -> (fsttoken2str [tk]) ^ "; " ^ (tlist2str l)
      |[] -> ""

  in "[" ^ (tlist2str l) ^ "]"

let int2str n =
  let digit2str d = if (d >= 0) && (d < 10) then String.make 1 (Char.chr ((Char.code '0') + d)) else failwith "[Digit2Str] Parameter is not a digit(0-9)"
  in let rec int2str n = if n < 1 then "" else (int2str (n / 10)) ^ (digit2str (n mod 10))
  in int2str n

let val2str v =
  let bool2str b = match b with
    |true -> "true"
    |false -> "false"

  in let rec val2str v = match v with
    |Bval(b) -> bool2str b
    |Ival(n) -> int2str n
    |Closure(v, e, env) -> "(" ^ v ^ ", <exp>, <env>)"
    |Rclosure(var1, var2, e, env) -> "(" ^ var1 ^ "," ^ var2 ^ ", <exp>, <env>)"
    |Pair(v1, v2) -> "(" ^ (val2str v1) ^ ", " ^ (val2str v2) ^ ")"

  in val2str v

let op2str o = match o with
  |Add -> "+"
  |Sub -> "-"
  |Mul -> "*"
  |Leq -> "<="

(** Lexer **)

(*** Helper Functions ***)

let whitespace c = c = ' ' || c = '\n' || c = '\t'
let digit c = (Char.code c) >= (Char.code '0') && (Char.code c) <= (Char.code '9')
let lc_letter c = (Char.code c) >= (Char.code 'a') && (Char.code c) <= (Char.code 'z')
let uc_letter c = (Char.code c) >= (Char.code 'A') && (Char.code c) <= (Char.code 'Z')
let char2digit c = (Char.code c) - (Char.code '0')

(*** Main Functions ***)

let lex s : token list =
  let get i = String.get s i in
  let getstr i n = String.sub s (i-n) n in
  let exhausted i = i >= String.length s in
  let verify i c = not (exhausted i) && get i = c in
  let rec lex i l =
    if exhausted i then List.rev l
    else match get i with
      |'-' -> if (verify (i+1) '>') then lex (i+2) (ARR::l) else lex (i+1) (SUB::l)
      |'<' -> if (verify (i+1) '=') then lex (i+2) (LEQ::l) else failwith ("[Lex] Illegal Character or EOL")
      |'+' -> lex (i+1) (ADD::l)
      |'*' -> lex (i+1) (MUL::l)
      |'(' -> begin match verify (i+1) '*' with
        |true -> lex_ct (i+1) l 1
        |false -> lex (i+1) (LP::l) end
      |')' -> lex (i+1) (RP::l)
      |',' -> lex (i+1) (COM::l)
      |'=' -> lex (i+1) (EQ::l)
      |':' -> lex (i+1) (COL::l)
      |c when whitespace c -> lex (i+1) l
      |c when digit c -> lex_num (i+1) (char2digit c) l
      |c when lc_letter c -> lex_id (i+1) 1 l
      |c -> failwith ("[Lex] Illegal Character: '" ^ (String.make 1 c) ^ "'")

  and lex_ct i l d = match exhausted i with
    |true -> failwith "[Lex] Comment reached EOL (Missing '*)' ?)"
    |false -> lex_ct' i l d

  and lex_ct' i l d = match get i with
    |'*' -> begin match verify (i+1) ')' with
      |true -> if d <= 1 then lex (i+2) l else lex_ct (i+1) l (d-1)
      |false -> lex_ct (i+1) l d end
    |'(' -> begin match verify (i+1) '*' with
      |true -> lex_ct (i+1) l (d+1)
      |false -> lex_ct (i+1) l d end
    |_ -> lex_ct (i+1) l d

  and lex_num i n l = if exhausted i then lex_num' i n l
    else match get i with
      |c when digit c -> lex_num (i+1) ((n*10) + (char2digit c)) l
      |_ -> lex_num' i n l

  and lex_num' i n l = lex i (CON(ICON n)::l)

  and lex_id i n l = if exhausted i then lex_id' i n l
    else match get i with
      |'_' |'\'' -> lex_id (i+1) (n+1) l
      |c -> if lc_letter c || uc_letter c || digit c
        then lex_id (i+1) (n+1) l
        else lex_id' i n l

  and lex_id' i n l = match getstr i n with
    |"if" -> lex i (IF::l)
    |"then" -> lex i (THEN::l)
    |"else" -> lex i (ELSE::l)
    |"fun" -> lex i (LAM::l)
    |"let" -> lex i (LET::l)
    |"in" -> lex i (IN::l)
    |"rec" -> lex i (REC::l)
    |"true" -> lex i (CON(BCON(true))::l)
    |"false" -> lex i (CON(BCON(false))::l)
    |"int" -> lex i (INT::l)
    |"bool" -> lex i (BOOL::l)
    |s -> lex i (VAR(s)::l)
  in lex 0 []


(* Parser *)

let parsetype l : ty * (token list) =
  let rec ty l = 
    let (t, l) = pty l
    in ty' t l
    
  and ty' t l = match l with
    |ARR::l -> begin
      let (t1, l) = pty l
      in let (t2, l) = ty' t1 l
      in ((Arrow(t, t2)), l) end
    |tk -> pairty t l

  and pairty t l = match l with
    |MUL::l -> begin
      let (t1, l) = pty l in
      ty' (Pair(t, t1)) l end
    |_ -> (t, l)
  
  and pty l = match l with
    |BOOL::l -> (Bool, l)
    |INT::l -> (Int, l)
    |LP::l -> begin
      let (t, l) = ty l
      in begin match l with
        |RP::l -> (t, l)
        |_ -> failwith "[Parsetype] Missing closing parenthesis" end end
    |l -> failwith ("[Parsetype] Unexpected Token: " ^ (fsttoken2str l))
  
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
        |l -> failwith ("[Parse] Lam: Unexpected Token: Expected ')' and '->', but got " ^ (fsttoken2str l)) end
    |LET::VAR(x)::EQ::l -> begin
      let (e1, l) = exp l in
      match l with
        |IN::l -> begin
          let (e2, l) = exp l in
          (Let(x, e1, e2), l) end
        |l -> failwith ("[Parse] Let: Unexpected Token: Expected 'in', but got " ^ (fsttoken2str l)) end
    |LET::LP::VAR(x1)::COM::VAR(x2)::RP::EQ::l -> begin
      let (e1, l) = exp l in
      match l with
        |IN::l -> begin
          let (e2, l) = exp l in
          (Letpair(x1, x2, e1, e2), l) end
        |l -> failwith ("[Parse] Letpair: Unexpected Token: Expected 'in', but got " ^ (fsttoken2str l)) end
    |LET::REC::VAR(f)::VAR(x)::EQ::l -> begin
      let (e1, l) = exp l in
      match l with
        |IN::l -> begin
          let (e2, l) = exp l in
          (Let(x, e1, e2), l) end
        |l -> failwith ("[Parse] Letrec: Unexpected Token: Expected 'in', but got " ^ (fsttoken2str l)) end
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
                  |l -> failwith ("[Parse] Letrecty: Unexpected Token: Expected 'in', but got " ^ (fsttoken2str l)) end
              |l -> failwith ("[Parse] Letrecty: Unexpected Token: Expected '=', but got " ^ (fsttoken2str l)) end
        |l -> failwith ("[Parse] Letrecty: Unexpected Token: Expected ')' and ':', but got " ^ (fsttoken2str l)) end
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
          |l -> failwith ("[Parse] If: Unexpected Token: Expected 'else', but got " ^ (fsttoken2str l)) end
      |_ -> failwith ("[Parse] IF: Unexpected Token: Expected 'then', but got " ^ (fsttoken2str l))

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
        |COM::l -> begin
          let (e2, l) = exp l in
          match l with
            |RP::l -> (Pairexp(e, e2), l)
            |l -> failwith ("[Parse] Missing parenthesis: Expected ')', but got " ^ (fsttoken2str l)) end
        |l -> failwith ("[Parse] Missing parenthesis: Expected ')' or ',', but got " ^ (fsttoken2str l)) end end
    |l -> failwith ("[Parse] Unexpected Token: " ^ (tlist2str l))

  in exp l


(** Type Checker **)

let check env e : ty =
  let check_op (o: op) t1 t2 : ty = match o with
    |Add -> if (t1 = Int) && (t2 = Int) then Int else failwith ("[Check] Oapp: Wrong type(s): Expected 'int' and 'int', got " ^ (ty2str t1) ^ " and " ^ (ty2str t2))
    |Sub -> if (t1 = Int) && (t2 = Int) then Int else failwith ("[Check] Oapp: Wrong type(s): Expected 'int' and 'int', got " ^ (ty2str t1) ^ " and " ^ (ty2str t2))
    |Mul -> if (t1 = Int) && (t2 = Int) then Int else failwith ("[Check] Oapp: Wrong type(s): Expected 'int' and 'int', got " ^ (ty2str t1) ^ " and " ^ (ty2str t2))
    |Leq -> if (t1 = Int) && (t2 = Int) then Bool else failwith ("[Check] Oapp: Wrong type(s): Expected 'int' and 'int', got " ^ (ty2str t1) ^ " and " ^ (ty2str t2)) in
  
  let check_fun t1 t2 = match t1 with
    |Arrow(ty1, ty2) -> if t2 = ty1 then ty2 else failwith ("[Check] Fapp: Argument type and given type don't match: Expected " ^ (ty2str ty1) ^ ", got " ^ (ty2str t2))
    |ty -> failwith ("[Check] Fapp: Expected: Expected a function, got expression of type " ^ (ty2str ty)) in

  let rec check env e : ty = match e with
    |Var(x) -> begin match lookup env x with
      |Some t -> t
      |None -> failwith ("[Check] Var: Type for variable " ^ x ^ " not defined") end
    |Con(Bcon(b)) -> Bool
    |Con(Icon(i)) -> Int
    |Pairexp(e1, e2) -> Pair((check env e1), (check env e2))
    |Oapp(o, e1, e2) -> check_op o (check env e1) (check env e2)
    |Fapp(e1, e2) -> check_fun (check env e1) (check env e2)
    |If(e1, e2, e3) -> begin match (check env e1) with
      |Bool -> begin let t1 = check env e2 in
        let t2 = check env e3 in
        if t1 = t2 then t1
        else failwith ("[Check] If: then and else expressions are not of the same type: Got " ^ (ty2str t1) ^ " and " ^ (ty2str t2)) end
      |ty -> failwith ("[Check] If: Condition is not of type 'bool': Got " ^ (ty2str ty)) end
    |Lam(x, e) -> failwith "[Check] Lam: Missing type declaration for parameter"
    |Lamty(x, t, e) -> Arrow (t, check (update env x t) e)
    |Let(x, e1, e2) -> check (update env x (check env e1)) e2
    |Letpair(x1, x2, e1, e2) -> begin let ty = check env e1 in
      match ty with
        |Pair(ty1, ty2) -> check (update (update env x1 ty1) x2 ty2) e2
        |ty -> failwith ("[Check] Letpair: Expected expression of type 'Pair', got expression of type " ^ (ty2str ty)) end
    |Letrec(f, x, e1, e2) -> failwith "[Check] Lam: Missing type declarations for parameter and return value"
    |Letrecty(f, x, t1, t2, e1, e2) -> let t2' = (check (update (update env x t1) f (Arrow(t1, t2))) e1) in
      if t2 = t2' then check (update env f (Arrow(t1, t2))) e2
      else failwith ("[Check] Return type of Function (" ^ (ty2str t2') ^ ") does not match expected return type (" ^ (ty2str t2) ^ ")")
  in check env e


(** Evaluator **)

let eval (env : (var, value) env) e =
  let rec eval env e = match e with
    |Var(x) -> begin match lookup env x with
      |Some v -> v
      |None -> failwith ("[Eval] Undefined Variable: " ^ x) end
    |Con(Bcon b) -> Bval b
    |Con(Icon n) -> Ival n
    |Pairexp(e1, e2) -> Pair((eval env e1), (eval env e2))
    |Oapp(o, e1, e2) -> eval_op o (eval env e1) (eval env e2)
    |Fapp(e1, e2) -> eval_fun (eval env e1) (eval env e2)
    |If(e1, e2, e3) -> begin match eval env e1 with
      |Bval(b) -> begin match b with
        |true -> eval env e2
        |false -> eval env e3 end
      |v -> failwith ("[Eval] If: Expected 'Bool' value, but got: " ^ (val2str v)) end
    |Lam(x, e) |Lamty(x, _, e) -> Closure(x, e, env)
    |Let(x, e1, e2) -> eval (update env x (eval env e1)) e2
    |Letpair(x1, x2, e1, e2) -> begin let v = eval env e1 in
      match v with
        |Pair(v1, v2) -> eval (update (update env x1 v1) x2 v2) e2
        |v -> failwith ("[Eval] Letpair: Expected 'Pair' value, but got: " ^ (val2str v)) end
    |Letrec(f, x, e1, e2) |Letrecty(f, x, _, _, e1, e2) -> eval (update env f (Rclosure(f, x, e1, env))) e2
  
  and eval_op op v1 v2 = match op, v1, v2 with
    |Add, Ival(n1), Ival(n2) -> Ival(n1 + n2)
    |Sub, Ival(n1), Ival(n2) -> Ival(n1 - n2)
    |Mul, Ival(n1), Ival(n2) -> Ival(n1 * n2)
    |Leq, Ival(n1), Ival(n2) -> Bval(n1 <= n2)
    |o, _, _ -> failwith ("[Eval] Oapp: Illegal values for operation " ^ (op2str o))

  and eval_fun v1 v2 = match v1 with
    |Closure(x, e, env) -> eval (update env x v2) e
    |Rclosure(f, x, e, env) -> eval (update (update env x v2) f v1) e
    |v -> failwith ("[Eval] Fapp: Expected function, got: " ^ (val2str v))

in eval env e


(** Interpreter **)

let checkStr s =
  let l = lex s in
  let e = match parse l with
    |(e, l) -> begin match l with
      |[] -> e
      |_ -> failwith "[checkStr] Token list not empty" end
  in check empty e

let evalStr s =
  let l = lex s in
  let e = match parse l with
    |(e, l) -> begin match l with
      |[] -> e
      |_ -> failwith "[evalStr] Token list not empty" end
  in eval empty e

let interpretStr s =
  let l = lex s in
  let e = match parse l with
    |(e, l) -> begin match l with
      |[] -> e
      |_ -> failwith "[interpretStr] Token list not empty" end in
  let ty = check empty e in
  let v = eval empty e in
  (ty2str ty) ^ " : " ^ (val2str v)