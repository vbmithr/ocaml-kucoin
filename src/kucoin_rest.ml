open Json_encoding
open Kucoin
open Core

(* https://api.kucoin.com *)
let url = Uri.make ~scheme:"https" ~host:"api.kucoin.com" ()

(* https://openapi-sandbox.kucoin.com *)
let sandbox_url = Uri.make ~scheme:"https" ~host:"openapi-sandbox.kucoin.com" ()

type l2book = {
  seq: int64 ;
  ts: Ptime.t ;
  bids: level list ;
  asks: level list ;
}
and level = { price: float ; size: float }
[@@deriving sexp]

let level =
  conv (fun _ -> assert false) (fun (price, size) -> { price; size }) (tup2 strfl strfl)

let err =
  conv
    (fun _ -> assert false)
    (fun (code, msg) -> Error.createf "%Ld: %s" code msg)
    (obj2 (req "code" strint) (req "msg" string))

let ok a =
  conv
    (fun _ -> assert false)
    (fun (_code, data) -> data)
    (obj2 (req "code" strint) (req "data" a))

let or_error a =
  union [
    case err (function Error e -> Some e | _ -> None) (fun e -> Error e) ;
    case (ok a) (function Ok a -> Some a | _ -> None) (fun a -> Ok a) ;
  ]

let l2book =
  conv
    (fun _ -> assert false)
    (fun (seq, ts, bids, asks) -> { seq; ts; bids; asks })
    (obj4
       (req "sequence" strint)
       (req "time" time_ms)
       (req "bids" (list level))
       (req "asks"(list level)))

let book20 ?(sandbox=false) symbol =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v1/market/orderbook/level2_20" in
  let url = Uri.with_query' url ["symbol", Pair.to_string symbol] in
  Fastrest.get (or_error l2book) url

let book100 ?(sandbox=false) symbol =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v1/market/orderbook/level2_100" in
  let url = Uri.with_query' url ["symbol", Pair.to_string symbol] in
  Fastrest.get (or_error l2book) url

let book ?(sandbox=false) symbol =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v2/market/orderbook/level2" in
  let url = Uri.with_query' url ["symbol", Pair.to_string symbol] in
  Fastrest.get (or_error l2book) url
