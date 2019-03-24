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

(* 8.1 *)
type book_t = {
  title: string;
  publisher: string;
  price: int;
  isbn: string;
}

let book_a = { title = "プログラミングの基礎"; publisher = "サイエンス社"; price = 2300; isbn = "ISBN978-4-7819-1160-1" }
let book_b = { title = "入門 監視"; publisher = "オライリー・ジャパン"; price = 2800; isbn = "ISBN978-4-87311-864-2" }
let book_c = { title = "ソフトウェアテスト技法ドリル"; publisher = "日科技連"; price = 2800; isbn = "ISBN978-4-8171-9360-5" }

(* 8.2 *)
type okozukai_t = {
  name: string;
  price: int;
  place: string;
  date: string;
}

let buy_a = { name = "消しゴム"; price = 100; place = "本屋さん"; date = "2019/01/23" }
let buy_b = { name = "鉛筆"; price = 20; place = "コンビニエンス・ストア"; date = "2019/03/03" }
let buy_c = { name = "万年筆"; price = 25800; place = "デパート"; date = "2018/12/24" }

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
