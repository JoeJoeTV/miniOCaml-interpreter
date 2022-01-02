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

let fac = 
  fun n -> 
    let rec fac' a = 
      fun n -> 
        if n <= 0 then a else fac' (a * n) (n - 1)
    in fac' 1 n
in fac 5

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
  Var("fac")
)


let rec ty2str t : string =
  match t with
    |



let check_op (o: op) t1 t2 : ty = 
  match o with
    |Add -> if (t1 = Int) && (t2 = Int) then Int else failwith "Oapp: Wrong type(s) for this operation"
    |Sub -> if (t1 = Int) && (t2 = Int) then Int else failwith "Oapp: Wrong type(s) for this operation"
    |Mul -> if (t1 = Int) && (t2 = Int) then Int else failwith "Oapp: Wrong type(s) for this operation"
    |Leq -> if (t1 = Int) && (t2 = Int) then Bool else failwith "Oapp: Wrong type(s) for this operation";;
      
let check_fun t1 t2 = match t1 with
  |Arrow(ty1, ty2) -> if t2 = ty1 then ty2 else failwith "Fapp: Expression of type ty1 expected, got t2"
  |_ -> failwith "Fapp: Not a function";;

let rec check env e : ty = match e with
  |Var(x) -> begin match lookup env x with
    |Some t -> t
    |None -> failwith ("Undeclared type for variable " ^ x) end
  |Con(Bcon(b)) -> Bool
  |Con(Icon(i)) -> Int
  |Oapp(o, e1, e2) -> check_op o (check env e1) (check env e2)
  |Fapp(e1, e2) -> check_fun (check env e1) (check env e2)
  |If(e1, e2, e3) -> begin match (check env e1) with
    |Bool -> begin let t1 = check env e2 in
      let t2 = check env e3 in
      if t1 = t2 then t1
      else failwith "If: Expressions are not of the same type" end
    |_ -> failwith "If: Condition is not of type bool" end
  |Lam(x, e) -> failwith "fun: Missing type"
  |Lamty(x, t, e) -> Arrow (t, check (update env x t) e)
  |Let(x, e1, e2) -> check (update env x (check env e1)) e2
  |Letrec(f, x, e1, e2) -> failwith "let rec: Missing types"
  |Letrecty(f, x, t1, t2, e1, e2) -> let t2' = (check (update (update env x t1) f (Arrow(t1, t2))) e1) in
      if t2 = t2' then check (update env f (Arrow(t1, t2))) e2
      else failwith "let rec: WAT?";;
