open Compile
open Instruction
open Expr
open Runner
open Printf
open OUnit2
open ExtLib
open Color

let rec take l n =
  if n == 0 then []
  else
    match l with
      | [] -> failwith "took too many"
      | x::xs -> x::(take xs (n - 1))


let t name program expected = name>::test_run program name expected;;
let tvg name program expected = name>::test_run_valgrind program name expected;;
let terr name program expected = name>::test_err program name expected;;
let tdep name program expected = name>::fun ctxt ->
  let p = parse_string name program in
  let a = anf_program p in
  let answer = match a with
    | AProgram(decls, body) ->
      let deps = (dep_graph body) in
      deps in
  let c = Pervasives.compare in
  assert_equal (List.sort ~cmp:c expected) (List.sort ~cmp:c answer) ~printer:dump;;

let tcolor name program expected_colors = name>::fun ctxt ->
  let p = parse_string name program in
  let a = anf_program p in
  let body = match a with
    | AProgram(decls, body) -> body in
  let vars = getvars body in
  let edges = (dep_graph body) in
  let coloring = get_colors [] vars edges in
  let stackmax = List.fold_right (fun (_, l) m ->
    match l with
      | LStack(n) -> if n > m then n else m
      | LReg _ -> m) coloring 0 in
  assert_equal expected_colors stackmax ~printer:dump;;

let deps = [
  tdep "d0" "let x = 5 in x"
    [];
    
  tdep "simple" "let x = 5 in 
                 let y = 2 in
                 x + y"
    [("y", "x")];

  tdep "writeup1"
    "
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
x + y
    "
    [("m", "n"); ("x", "m"); ("x", "n"); ("y", "m"); ("y", "x")];

  tdep "writeup2"
    "
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
let z = x + y in
let k = z * z in
let g = k + 5 in
k + 3
    "
  [("g", "k"); ("k", "z"); ("m", "n"); ("x", "m"); ("x", "n"); ("y", "m"); ("y", "x"); ("z", "x"); ("z", "y")];

  tdep "tail_recursion"
  "
  def f(x, t):
  if x < 1: t
  else: f(x - 1, x + t)

  f(888888888, 0)
  "
  [];

  tdep "lecture"
  "
  let b = 4 in
  let x = 10 in
  let i = if true:
            let z = 11 in
              z + b
          else:
            let y = 9 in
              y + 1 in
  let a = 5 + i in
  a + x
  "
  [("a", "i"); ("a", "x"); ("i", "b"); ("i", "x"); ("x", "b"); ("y", "x"); ("z", "b"); ("z", "x")];

  tdep "overloadd"
  "
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  let e = 5 in
  let f = 6 in
  let g = 7 in
  let h = 8 in
  let i = 9 in
  let t1 = a + b in
  let t2 = t1 + c in
  let t3 = t2 + d in
  let t4 = t3 + e in
  let t5 = t4 + f in
  let t6 = t5 + g in
  let t7 = t6 + h in
  let t8 = t7 + i in
  t8
  "
  [("b", "a"); ("c", "a"); ("c", "b"); ("d", "a"); ("d", "b"); ("d", "c"); ("e", "a"); ("e", "b"); ("e", "c"); ("e", "d"); ("f", "a"); ("f", "b"); ("f", "c"); ("f", "d"); ("f", "e"); ("g", "a"); ("g", "b"); ("g", "c"); ("g", "d"); ("g", "e"); ("g", "f"); ("h", "a"); ("h", "b"); ("h", "c"); ("h", "d"); ("h", "e"); ("h", "f"); ("h", "g"); ("i", "a"); ("i", "b"); ("i", "c"); ("i", "d"); ("i", "e"); ("i", "f"); ("i", "g"); ("i", "h"); ("t1", "a"); ("t1", "b"); ("t1", "c"); ("t1", "d"); ("t1", "e"); ("t1", "f"); ("t1", "g"); ("t1", "h"); ("t1", "i"); ("t2", "c"); ("t2", "d"); ("t2", "e"); ("t2", "f"); ("t2", "g"); ("t2", "h"); ("t2", "i"); ("t2", "t1"); ("t3", "d"); ("t3", "e"); ("t3", "f"); ("t3", "g"); ("t3", "h"); ("t3", "i"); ("t3", "t2"); ("t4", "e"); ("t4", "f"); ("t4", "g"); ("t4", "h"); ("t4", "i"); ("t4", "t3"); ("t5", "f"); ("t5", "g"); ("t5", "h"); ("t5", "i"); ("t5", "t4"); ("t6", "g"); ("t6", "h"); ("t6", "i"); ("t6", "t5"); ("t7", "h"); ("t7", "i"); ("t7", "t6"); ("t8", "i"); ("t8", "t7")];

  tdep "let_in_decl"
  "
  def f(x, y):
    let z = x + y in
    z - z

  f(3, 6)
  "
  [];

  tdep "id_in_app"
  "
  def f(x, y):
  if x: let a = 4 in let b = 3 in a + b
  else: y

  let t = 40 in
  let u = 41 in
  f(t, u)
  "
  [("u", "t")];

  tdep "top_level_if"
  "
  if true:
    let x = 3 in
    x
  else:
    let z = 5 in
    let y = 6 in
    if false:
      z + y
    else:
      z
  "
  [("y", "z")];

  tdep "prim1s"
  "
  let z = IsNum(3) in
  let y = IsBool(5) in
  let x = Add1(z) in
  let t1 = Sub1(y) in
  let w = t1 + x in
  let t2 = z + t1 in
  let v = print(t2) in
  v
  "
  [("t1", "x"); ("t1", "y"); ("t1", "z"); ("t2", "t1"); ("t2", "z"); ("v", "t2"); ("w", "t1"); ("w", "x"); ("w", "z"); ("x", "y"); ("x", "z"); ("y", "z")];
]

let colors = [

  tcolor "c0" "let x = 5 in x"
    1;
    
  tcolor "simplec" "let x = 5 in 
                 let y = 2 in
                 x + y"
    2;

  tcolor "writeup1c"
    "
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
x + y
    "
    3;

  tcolor "writeup2c"
    "
let n = 5 * 5 in
let m = 6 * 6 in
let x = n + 1 in
let y = m + 1 in
let z = x + y in
let k = z * z in
let g = k + 5 in
k + 3
    "
    3;

  tcolor "tail_recursion"
  "
  def f(x, t):
  if x < 1: t
  else: f(x - 1, x + t)

  f(888888888, 0)
  "
  0;

  tcolor "lecture"
  "
  let b = 4 in
  let x = 10 in
  let i = if true:
            let z = 11 in
              z + b
          else:
            let y = 9 in
              y + 1 in
  let a = 5 + i in
  a + x
  "
  3;

  tcolor "overloadc"
  "
  let a = 1 in
  let b = 2 in
  let c = 3 in
  let d = 4 in
  let e = 5 in
  let f = 6 in
  let g = 7 in
  let h = 8 in
  let i = 9 in
  let t1 = a + b in
  let t2 = t1 + c in
  let t3 = t2 + d in
  let t4 = t3 + e in
  let t5 = t4 + f in
  let t6 = t5 + g in
  let t7 = t6 + h in
  let t8 = t7 + i in
  t8
  "
  10;

  tcolor "id_in_appc"
  "
  def f(x, y):
  if x: let a = 4 in let b = 3 in a + b
  else: y

  let t = 40 in
  let u = 41 in
  f(t, u)
  "
  2;

  tcolor "prim1sc"
  "
  let z = IsNum(3) in
  let y = IsBool(5) in
  let x = Add1(z) in
  let t1 = Sub1(y) in
  let w = t1 + x in
  let t2 = z + t1 in
  let v = print(t2) in
  v
  "
  4;
]

let suite =
"suite">::: deps @ colors

let () =
  run_test_tt_main suite
;;

