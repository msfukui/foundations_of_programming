(* 8.3 *)
type person_t = {
  name: string;
  tall_m: float;
  weight_k: float;
  birthday_month: int;
  birthday_day: int;
  blood_type: string;
}

let person_a = { name = "ふつうのこ"; tall_m = 1.655; weight_k = 72.0; birthday_month = 2; birthday_day = 3; blood_type = "B" }
let person_b = { name = "でかお"; tall_m = 1.802; weight_k = 68.2; birthday_month = 12; birthday_day = 5; blood_type = "AB" }
let person_c = { name = "ちっちゃいの"; tall_m = 1.552; weight_k = 48.3; birthday_month = 8; birthday_day = 23; blood_type = "A" }

(* 8.4 *)
(* 目的: person_t 型のデータを受け取り「name さんの血液型は blood_type 型です」という文字列を返す *)
(* ketsueki_hyoji: person_t -> string *)
let ketsueki_hyoji person = match person with
  { name = n; tall_m = t; weight_k = w; birthday_month = bm; birthday_day = bd; blood_type = bt } ->
    n ^ "さんの血液型は" ^ bt ^ "型です"

let test8_4_1 = ketsueki_hyoji person_a = "ふつうのこさんの血液型はB型です"
let test8_4_2 = ketsueki_hyoji person_b = "でかおさんの血液型はAB型です"
let test8_4_3 = ketsueki_hyoji person_c = "ちっちゃいのさんの血液型はA型です"

(* 9.7 *)
(* 目的: person_t 型のデータを受け取り、血液型が A 型の人の数を返す *)
(* count_ketsueki_A: person_t list -> int *)
let rec count_ketsueki_A lst = match lst with
    [] -> 0
  | {name = n; tall_m = t; weight_k = w; birthday_month = bm; birthday_day = bd; blood_type = bt} :: rest
    -> if bt = "A" then 1 + count_ketsueki_A rest
                   else count_ketsueki_A rest

(* テスト *)
let test9_7_1 = count_ketsueki_A [] = 0
let test9_7_2 = count_ketsueki_A [person_a] = 0
let test9_7_3 = count_ketsueki_A [person_a; person_b; person_c] = 1

(* 9.8 *)
(* 目的: person_t 型のデータを受け取り、生月日が乙女座の人の名前のみのリストを返す *)
(* otomeza: person_t list -> string list *)
let rec otomeza lst = match lst with
    [] -> []
  | {name = n; tall_m = t; weight_k = w; birthday_month = bm; birthday_day = bd; blood_type = bt} :: rest
    -> if (bm = 8 && bd > 22) || (bm = 9 && bd < 23) then n :: otomeza rest
                                                     else otomeza rest

(* テスト *)
let test9_8_1 = otomeza [] = []
let test9_8_2 = otomeza [person_a] = []
let test9_8_3 = otomeza [person_a; person_b; person_c] = ["ちっちゃいの"]

(* 10.4 *)
(* 目的 : あらかじめ名前順に並んでいる person_t のリストと新しい person_t を受け取ったら、
 * 並び順となる位置に person_t を挿入したリストを返す *)
(* insert : person_t list -> person_t -> person_t list *)
let rec insert_person lst p = match lst with
    [] -> p :: []
  | first :: rest ->
      if first.name > p.name then p :: first :: rest
      else first :: (insert_person rest p)

(* テスト *)
let test10_4_1 = insert_person [] person_a = [person_a]
let test10_4_2 = insert_person [person_b] person_a = [person_b; person_a]
let test10_4_3 = insert_person [person_b; person_a] person_c = [person_c; person_b; person_a]

(* 目的 : person_t のリストを受け取り、「挿入法」で name で昇順に整列したリストを返す *)
(* sort_person : person_t list -> person_t list *)
let rec person_sort lst = match lst with
    [] -> []
  | first :: rest -> insert_person (person_sort rest) first

(* テスト *)
let test10_4_4 = person_sort [] = []
let test10_4_5 = person_sort [person_a; person_b] = [person_b; person_a]
let test10_4_6 = person_sort [person_a; person_b; person_c] = [person_c; person_b; person_a]

(* 問題10.7 *)
(* 目的: 人のリスト lst を受け取ったら描く血液型の人が何人いるかを組にして返す *)
(* ketsueki_shukei : person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {name = n; tall_m = t; weight_k = w; birthday_month = bm; birthday_day = bd; blood_type = bt} :: rest ->
      let (a, b, o, ab) = ketsueki_shukei rest in
      if bt = "A" then (a + 1, b, o, ab)
      else if bt = "B" then (a, b + 1, o, ab)
      else if bt = "O" then (a, b, o + 1, ab)
      else (a, b, o, ab + 1)

(* テスト *)
let test10_7_1 = ketsueki_shukei [] = (0, 0, 0, 0)
let test10_7_2 = ketsueki_shukei [person_a] = (0, 1, 0, 0)
let test10_7_3 = ketsueki_shukei [person_a; person_b; person_c] = (1, 1, 0, 1)

(* 問題10.8 *)
(* 目的: 人のリスト lst を受け取ったら最も多い血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let saita_ketsueki lst =
  let (a, b, o, ab) = ketsueki_shukei lst in
  if a >= b then
    if a >= o then
      if a >= ab then "A"
      else "AB"
    else if b >= o then
      if b >= ab then "B"
      else "AB"
    else if o >= ab then "O"
    else "AB"
  else
    if b >= o then
      if b >= ab then "B"
      else "AB"
    else if o >= ab then "O"
    else "AB"

(* テスト *)
let test10_8_1 = saita_ketsueki [] = "A"
let test10_8_2 = saita_ketsueki [person_a] = "B"
let test10_8_3 = saita_ketsueki [person_a; person_b; person_c] = "A"
