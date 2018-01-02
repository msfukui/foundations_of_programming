(* 目的: 鶴の足の数 x に対して足の本数を計算する *)
(* tsuru_no_ashi int -> int *)
let tsuru_no_ashi x = x * 2

(* tsuru_no_ashi テスト *)
let tsuru_no_ashi_test1 = tsuru_no_ashi 2 = 4
let tsuru_no_ashi_test2 = tsuru_no_ashi 3 = 6
let tsuru_no_ashi_test3 = tsuru_no_ashi 4 = 8

(* 目的: 亀の足の数 x に対して足の本数を計算する *)
(* kame_no_ashi int -> int *)
let kame_no_ashi x = x * 4

(* kame_no_ashi テスト *)
let kame_no_ashi_test1 = kame_no_ashi 2 = 8
let kame_no_ashi_test2 = kame_no_ashi 3 = 12
let kame_no_ashi_test3 = kame_no_ashi 4 = 16

(* 目的: 鶴の足の数 x と 亀の足の数 y に対して足の本数を計算する *)
(* tsurukame_no_ashi int -> int -> int *)
let tsurukame_no_ashi x y = tsuru_no_ashi x + kame_no_ashi y

(* tsurukame_no_ashi テスト *)
let tsurukame_no_ashi_test1 = tsurukame_no_ashi 2 3 = 16
let tsurukame_no_ashi_test2 = tsurukame_no_ashi 3 4 = 22
let tsurukame_no_ashi_test3 = tsurukame_no_ashi 4 4 = 24

(* 目的: 鶴 x と亀 y の数の合計 m と鶴と亀の足の数の合計 n に対して鶴の数を計算する *)
(* 2 * x + 4 * y = n *)
(* x + y = m *)
(* tsurukame int -> int -> int *)
let tsurukame m n = 2 * m - n / 2

(* tsurukame テスト *)
let tsurukame_test1 = tsurukame 5 16 = 2
let tsurukame_test2 = tsurukame 7 22 = 3
let tsurukame_test3 = tsurukame 8 24 = 4
