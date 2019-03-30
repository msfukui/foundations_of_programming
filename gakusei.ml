(* 学生ひとり分のデータ（名前、点数、成績）を表す型 *)
type gakusei_t = {
  namae: string;  (* 名前 *)
  tensuu: int;    (* 点数 *)
  seiseki: string (* 成績 *)
}
(* 目的: 学生のデータ gakusei を受け取り成績のついたデータを返す *)
(* hyokuka: gakusei_t -> gakusei_t *)
let hyouka gakusei = match gakusei with
  { namae = n; tensuu = t; seiseki = s } ->
    if t >= 80 then      { namae = n; tensuu = t; seiseki = "A" }
    else if t >= 70 then { namae = n; tensuu = t; seiseki = "B" }
    else if t >= 60 then { namae = n; tensuu = t; seiseki = "C" }
    else                 { namae = n; tensuu = t; seiseki = "D" }

(* テスト *)
let test1 = hyouka { namae = "asai"; tensuu = 90; seiseki = "" }
                 = { namae = "asai"; tensuu = 90; seiseki = "A" }
let test2 = hyouka { namae = "asai"; tensuu = 80; seiseki = "" }
                 = { namae = "asai"; tensuu = 80; seiseki = "A" }
let test3 = hyouka { namae = "asai"; tensuu = 75; seiseki = "" }
                 = { namae = "asai"; tensuu = 75; seiseki = "B" }
let test4 = hyouka { namae = "asai"; tensuu = 70; seiseki = "" }
                 = { namae = "asai"; tensuu = 70; seiseki = "B" }
let test5 = hyouka { namae = "asai"; tensuu = 65; seiseki = "" }
                 = { namae = "asai"; tensuu = 65; seiseki = "C" }
let test6 = hyouka { namae = "asai"; tensuu = 60; seiseki = "" }
                 = { namae = "asai"; tensuu = 60; seiseki = "C" }
let test7 = hyouka { namae = "asai"; tensuu = 55; seiseki = "" }
                 = { namae = "asai"; tensuu = 55; seiseki = "D" }

(* 9.6 *)
(* gakusei_t list は
 *  - []       空リスト、あるいは
 *  - first :: rest 最初の要素が first で残りのリストが rest
 *                  (first は gakusei_t 型、
 *                   rest が自己参照のケース)
 * という形 *)
(* gakusei_t list 型のデータの例 *)
let lst1 = []
let lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}]
let lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}]
let lst4 = [{namae = "yoshida"; tensuu = 80; seiseki = "A"};
            {namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}]

(* 目的: 学生リスト lst のうち成績が A の人の数を返す *)
(* count_A: gakusei_t list -> int *)
let rec count_A lst = match lst with
    [] -> 0
  | {namae = n; tensuu = t; seiseki = s} :: rest
    -> if s = "A" then 1 + count_A rest
                  else count_A rest

(* テスト *)
let test1 = count_A lst1 = 0
let test2 = count_A lst2 = 0
let test3 = count_A lst3 = 1
let test4 = count_A lst4 = 2

(* 10.3 *)
let sample1 = {namae = "yoshida"; tensuu = 80; seiseki = "A"}
let sample2 = {namae = "asai"; tensuu = 70; seiseki = "B"}
let sample3 = {namae = "kaneko"; tensuu = 85; seiseki = "A"}

(* 目的 : あらかじめ tensuu 順に昇順に並んでいる gakusei_t のリストと新しい gakusei_t を受け取ったら、
 * 昇順となる位置に gakusei_t を挿入したリストを返す *)
(* insert : gakusei_t list -> gakusei_t -> gakusei_t list *)
let rec insert_gakusei lst g = match lst with
    [] -> g :: []
  | first :: rest ->
      if first.tensuu > g.tensuu then g :: first :: rest
      else first :: (insert_gakusei rest g)

(* テスト *)
let test10_3_1 = insert_gakusei [] sample1 = [sample1]
let test10_3_2 = insert_gakusei [sample1] sample2 = [sample2; sample1]
let test10_3_3 = insert_gakusei [sample2; sample1] sample3 = [sample2; sample1; sample3]

(* 目的 : gakusei_t のリストを受け取り、「挿入法」で tensuu で昇順に整列したリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst = match lst with
    [] -> []
  | first :: rest -> insert_gakusei (gakusei_sort rest) first

(* テスト *)
let test10_3_4 = gakusei_sort [] = []
let test10_3_5 = gakusei_sort [sample1; sample2] = [sample2; sample1]
let test10_3_6 = gakusei_sort [sample1; sample2; sample3] = [sample2; sample1; sample3]
