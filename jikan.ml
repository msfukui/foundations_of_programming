(* 目的: 時間 x を受け取ったら午前か午後を返却する *)
(* jikan : int -> string *)
let jikan x =
  if (x mod 24) < 12 then "午前"
                     else "午後"

(* テスト *)
let test1 = jikan 0 = "午前"
let test2 = jikan 11 = "午前"
let test3 = jikan 12 = "午後"
let test4 = jikan 13 = "午後"
let test5 = jikan 23 = "午後"
let test6 = jikan 24 = "午前"
let test7 = jikan 25 = "午前"
let test8 = jikan 38 = "午後"

(* x が負数の場合は? *)
