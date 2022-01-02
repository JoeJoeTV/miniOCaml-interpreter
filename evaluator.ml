(* Types *)
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

type ('a,'b) env = ('a * 'b) list;;



(* Code*)

let empty : ('a,'b) env = []
let update (env : ('a,'b) env) a b : ('a,'b) env = (a,b) :: env
let rec lookup (env : ('a,'b) env) a =  match env with
  | (a',b) :: env -> if a = a' then Some b else lookup env a
  | [] -> None

type value = Bval of bool | Ival of int
  | Closure of var * exp * (var, value) env
  | Rclosure of var * var * exp * (var, value) env


let rec eval (env : (var, value) env) e = match e with
  |Var(x) -> begin match lookup env x with
    |Some v -> v
    |None -> failwith ("Var: Undefined Variable " ^ x) end
  |Con(Bcon b) -> Bval b
  |Con(Icon n) -> Ival n
  |Oapp(o, e1, e2) -> eval_op o (eval env e1) (eval env e2)
  |Fapp(e1, e2) -> eval_fun (eval env e1) (eval env e2)
  |If(e1, e2, e3) -> begin match eval env e1 with
    |Bval(b) -> begin match b with
      |true -> eval env e2
      |false -> eval env e3 end
    |_ -> failwith "If: Value not a Boolean" end
  |Lam(x, e) |Lamty(x, _, e) -> Closure(x, e, env)
  |Let(x, e1, e2) -> eval (update env x (eval env e1)) e2
  |Letrec(f, x, e1, e2) |Letrecty(f, x, _, _, e1, e2) -> eval (update env f (Rclosure(f, x, e1, env))) e2

and eval_op op v1 v2 = match op, v1, v2 with
  |Add, Ival(n1), Ival(n2) -> Ival(n1 + n2)
  |Sub, Ival(n1), Ival(n2) -> Ival(n1 - n2)
  |Mul, Ival(n1), Ival(n2) -> Ival(n1 * n2)
  |Leq, Ival(n1), Ival(n2) -> Bval(n1 <= n2)
  |_, _, _ -> failwith "Oapp: Wrong Value type(s)"

and eval_fun v1 v2 = match v1 with
  |Closure(x, e, env) -> eval (update env x v2) e
  |Rclosure(f, x, e, env) -> eval (update (update env x v2) f v1) e
  |_ -> failwith "Fapp: Not a Function"


let mfac = Let("fac",
  Lamty(
    "n",
    Int,
    Letrecty(
      "fac'",
      "a",
      Int,
      Arrow(Int,Int),
      Lamty(
        "n",
        Int,
        If(
          Oapp(Leq,Var("n"),Con(Icon(0))),
          Var("a"),
          Fapp(Fapp(Var("fac'"),Oapp(Mul,Var("a"),Var("n"))),Oapp(Sub,Var("n"),Con(Icon(1))))
        )
      ),
      Fapp(Fapp(Var("fac'"), Con(Icon(1))),Var("n"))
    )
  ),
  Fapp(Var("fac"),Con(Icon(5)))
);;

let test = 
  let v = eval empty mfac in
  match v with
    |Ival(n) -> print_string "RESULT: "; print_int n; print_newline ()
    |_ -> print_string "WAT"; print_newline ();;


test;;