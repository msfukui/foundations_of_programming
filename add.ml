(* 7.1 *)
(* 目的: 国語と数学と英語と理科と社会の点数から合計点と平均点の組を返す *)
(* bmi : float -> float -> float -> float -> float -> float * float *)
let goukei_to_heikin kokugo sugaku eigo rika syakai =
  (kokugo +. sugaku +. eigo +. rika +. syakai, (kokugo +. sugaku +. eigo +. rika +. syakai) /. 5.)

let test1_1 = goukei_to_heikin 40. 41. 42. 43. 44. = (210., 42.)
let test1_2 = goukei_to_heikin 40. 41. 42. 44. 45. = (212., 42.4)

(* 7.2 *)
(* 目的: 名前と成績の組を受け取ったら「xさんの評価はyです」という文字列を返す *)
(* seiseki : string * string -> string *)
let seiseki pair = match pair with
  (name, result) -> name ^ "さんの評価は" ^ result ^ "です"

let test2_1 = seiseki ("ルーク", "A+") = "ルークさんの評価はA+です"

(* 目的: ふたつの整数の組 pair を受け取りその要素の和を返す *)
(* add : int * int -> int *)
let add pair = match pair with
  (a, b) -> a + b

let test1 = add (0, 0) = 0
let test2 = add (3, 5) = 8
let test3 = add (3, -5) = -2

(* 7.3 *)
(* 目的: 平面座標x, yからx軸について対称な点の座標を返す *)
(* taisho_x : int * int -> int * int *)
let taisho_x point = match point with
  (x, y) -> (-x, y)

let test3_1 = taisho_x (0, 0) = (0, 0)
let test3_2 = taisho_x (2, 3) = (-2, 3)
let test3_3 = taisho_x (2, -4) = (-2, -4)
let test3_3 = taisho_x (-3, -2) = (3, -2)

(* 7.4 *)
(* 目的: 平面座標x1, y1とx2, y2からその中点の座標を返す *)
(* chuten : float * float -> float * float -> float * float *)
let chuten point1 point2 = match point1, point2 with
  (x1, y1), (x2, y2) -> ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)

let test4_1 = chuten (0., 0.) (0., 0.) = (0., 0.)
let test4_2 = chuten (1., 2.) (3., 4.) = (2., 3.)
let test4_3 = chuten (1., 2.) (3., -4.) = (2., -1.)
let test4_4 = chuten (1., 2.) (-3., 4.) = (-1., 3.)
let test4_5 = chuten (1., 2.) (-3., -4.) = (-1., -1.)
let test4_6 = chuten (1., 1.) (2., 2.) = (1.5, 1.5)
