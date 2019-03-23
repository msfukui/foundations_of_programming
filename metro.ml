(* 駅名の情報 *)
type ekimei_t = {
  kanji: string;   (* 駅名 *)
  kana: string;    (* 駅名（かな） *)
  romaji: string;  (* 駅名（ローマ字） *)
  shozoku: string; (* 路線名 *)
}

(* 目的: 駅名の情報を受け取ったら「路線名、駅名（かな）」の文字列を返す *)
(* hyoji: ekimei_t -> string *)
let hyoji ekimei = match ekimei with
  { kanji = kj; kana = kn; romaji = r; shozoku = s } ->
    s ^ "、" ^ kj ^ "（" ^ kn ^ "）"

let test8_6_1_data = { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myougadani"; shozoku = "丸ノ内線" }
let test8_6_1 = hyoji test8_6_1_data = "丸ノ内線、茗荷谷（みょうがだに）"

(* 駅と駅の接続情報 *)
type ekikan_t = {
  kiten: string;  (* 起点の駅名 *)
  shuten: string; (* 終点の駅名 *)
  keiyu: string;  (* 経由する路線名 *)
  kyori: float;   (* 2駅間の距離（km） *)
  jikan: int;     (* 所要時間（分） *)
}
