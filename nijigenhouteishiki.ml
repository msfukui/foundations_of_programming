(* c.f. https://ja.wikipedia.org/wiki/%E4%BA%8C%E6%AC%A1%E6%96%B9%E7%A8%8B%E5%BC%8F *)

(* 目的: 二次元方程式 a * x^2 + b * x + c = 0 の a, b, c から判別式の値を返却する *)
(* hanbetsusiki : float -> float -> float -> float *)
let hanbetsushiki a b c =
  (b ** 2.0) -. (4.0 *. a *. c)

(* テスト: hanbetsushiki *)
let hanbetsushiki_test1 = hanbetsushiki 1.0 1.0 1.0 = -3.0
let hanbetsushiki_test2 = hanbetsushiki 2.0 3.0 4.0 = -23.0
let hanbetsushiki_test3 = hanbetsushiki 1.0 4.0 4.0 = 0.0
let hanbetsushiki_test4 = hanbetsushiki 1.0 4.0 2.0 = 8.0

(* 目的: 二次元方程式 a * x^2 + b * x + c = 0 の a, b, c から実数解の個数を返却する *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c =
  if      hanbetsushiki a b c > 0.0 then 2
  else if hanbetsushiki a b c = 0.0 then 1
  else                                   0

(* テスト: kai_no_kosuu *)
let kai_no_kosuu_test1 = kai_no_kosuu 1.0 1.0 1.0 = 0
let kai_no_kosuu_test2 = kai_no_kosuu 2.0 3.0 4.0 = 0
let kai_no_kosuu_test3 = kai_no_kosuu 1.0 4.0 4.0 = 1
let kai_no_kosuu_test4 = kai_no_kosuu 1.0 4.0 2.0 = 2

(* 目的: 二次元方程式 a * x^2 + b * x + c = 0 の a, b, c から虚数解を持つかどうかを判定する *)
(* kyosuukai : float -> float -> float -> bool *)
let kyosuukai a b c =
  kai_no_kosuu a b c = 0

(* テスト: kyosuukai *)
let kyosuukai_test1 = kyosuukai 1.0 1.0 1.0 = true
let kyosuukai_test2 = kyosuukai 2.0 3.0 4.0 = true
let kyosuukai_test3 = kyosuukai 1.0 4.0 4.0 = false
let kyosuukai_test4 = kyosuukai 1.0 4.0 2.0 = false
