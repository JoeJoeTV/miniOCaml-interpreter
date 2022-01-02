(*Types*)
type const = BCON of bool | ICON of int
type token = ADD | SUB | MUL | LP | RP | EQ | LEQ | ARR
  | IF | THEN | ELSE | LAM | LET | IN | REC
  | CON of const | VAR of string | BOOL | INT


(*Code*)
let whitespace c = c = ' ' || c = '\n' || c = '\t';;
let digit c = (Char.code c) >= (Char.code '0') && (Char.code c) <= (Char.code '9');;
let lc_letter c = (Char.code c) >= (Char.code 'a') && (Char.code c) <= (Char.code 'z');;
let uc_letter c = (Char.code c) >= (Char.code 'A') && (Char.code c) <= (Char.code 'Z');;

let char2digit c = (Char.code c) - (Char.code '0');;


let lex s : token list =
  let get i = String.get s i in
  let getstr i n = String.sub s (i-n) n in
  let exhausted i = i >= String.length s in
  let verify i c = not (exhausted i) && get i = c in
  let rec lex i l =
    if exhausted i then List.rev l
    else match get i with
      |'-' -> if (verify (i+1) '>') then lex (i+2) (ARR::l) else lex (i+1) (SUB::l) (*MAYBE REWITE WITH MATCH*)
      |'<' -> if (verify (i+1) '=') then lex (i+2) (LEQ::l) else failwith "Illegal Character"
      |'+' -> lex (i+1) (ADD::l)
      |'*' -> lex (i+1) (MUL::l)
      |'(' -> lex (i+1) (LP::l)
      |')' -> lex (i+1) (RP::l)
      |'=' -> lex (i+1) (EQ::l)
      |':' when lc_letter (get (i+1)) -> lex_id (i+1) 0 l
      |c when whitespace c -> lex (i+1) l
      |c when digit c -> lex_num (i+1) (char2digit c) l
      |c when lc_letter c -> lex_id (i+1) 1 l
      |_ -> failwith "DED1"

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
  in lex 0 [];;



let fac_string =
  "let rec fac a = fun n ->
    if n <= 1 then a else fac (n*a) (n-1) 
    in fac 1 5"

let test = lex fac_string
