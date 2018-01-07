(* 目的: 身長 x と体重 y から BMI 指数を返す *)
(* bmi : float -> float -> float *)
let bmi x y = y /. (x ** 2.0)

(* テスト: bmi *)
let bmi_test0 = bmi 1.655 70.0 = 25.55653927948814
let bmi_test1 = bmi 1.6 45.0 = 17.578124999999996
let bmi_test2 = bmi 1.6 50.0 = 19.531249999999996
let bmi_test3 = bmi 1.6 65.0 = 25.390624999999996
let bmi_test4 = bmi 1.6 77.0 = 30.078124999999993

(* 目的: 身長 x と体重 y から BMI 指数を元に体型を返す *)
(* taikei : float -> float -> string *)
let taikei x y =
  if      bmi x y >  0.0  && bmi x y < 18.5 then "やせ"
  else if bmi x y >= 18.5 && bmi x y < 25.0 then "標準"
  else if bmi x y >= 25.0 && bmi x y < 30.0 then "肥満"
  else if bmi x y >= 30.0                   then "高度肥満"
  else ""

(* テスト: taikei *)
let taikei_test0 = taikei 1.655 70.0 = "肥満"
let taikei_test1 = taikei 1.6 45.0 = "やせ"
let taikei_test2 = taikei 1.6 50.0 = "標準"
let taikei_test3 = taikei 1.6 65.0 = "肥満"
let taikei_test4 = taikei 1.6 77.0 = "高度肥満"
