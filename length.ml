(* 目的: 受け取ったリスト lst の長さを返す *)
(* length: int list -> int *)
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest

(* テスト *)
let test1 = length [] = 0
let test2 = length [2] = 1
let test3 = length [1; 3] = 2
let test4 = length [2; 1; 6; 4; 7] = 5
