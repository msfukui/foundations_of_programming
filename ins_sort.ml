(* 目的 : あらかじめ昇順に並んでいる整数のリストと整数を受け取ったら、
 * 昇順となる位置に整数を挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> n :: []
  | first :: rest ->
      if first > n then n :: first :: rest
      else first :: (insert rest n)

(* テスト *)
let test1 = insert [] 1 = [1]
let test2 = insert [1] 2 = [1; 2]
let test3 = insert [1; 3] 2 = [1; 2; 3]
let test4 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8]

(* 目的 : 整数のリストを受け取り、「挿入法」で昇順に整列したリストを返す *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let test5 = ins_sort [] = []
let test6 = ins_sort [1] = [1]
let test7 = ins_sort [3; 1] = [1; 3]
let test8 = ins_sort [8; 3; 7; 4; 1] = [1; 3; 4; 7; 8]
