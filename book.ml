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
