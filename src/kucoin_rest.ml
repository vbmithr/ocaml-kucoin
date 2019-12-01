open Json_encoding
open Kucoin
open Core

(* https://api.kucoin.com *)
let url = Uri.make ~scheme:"https" ~host:"api.kucoin.com" ()

(* https://openapi-sandbox.kucoin.com *)
let sandbox_url = Uri.make ~scheme:"https" ~host:"openapi-sandbox.kucoin.com" ()

type l2 = { price: float ; size: float } [@@deriving sexp]
type l3 = { orderID: Uuidm.t ; price: float ; size: float } [@@deriving sexp]

type 'a book = {
  seq: int64 ;
  ts: Ptime.t ;
  bids: 'a list ;
  asks: 'a list ;
} [@@deriving sexp]

let l2 =
  conv (fun _ -> assert false) (fun (price, size) -> { price; size }) (tup2 strfl strfl)
let l3 =
  conv
    (fun _ -> assert false)
    (fun (orderID, price, size) ->
       let orderID = orderID_of_hex (`Hex orderID) in
       { orderID; price; size }) (tup3 string strfl strfl)

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

type symbol = {
  id: string ;
  base_currency: string ;
  quote_currency: string ;
  base_min_size: float ;
  base_max_size: float ;
  base_increment: float ;
  quote_increment: float ;
} [@@deriving sexp]

let pair_of_symbol { base_currency ; quote_currency ; _ } =
  { Pair.base = base_currency ; quote = quote_currency }

let symbol =
  conv
    (fun { id; base_currency; quote_currency; base_min_size; base_max_size;
           base_increment; quote_increment } ->
      (),
      (id, base_currency, quote_currency, base_min_size, base_max_size,
       base_increment, quote_increment))
    (fun ((),
          (id, base_currency, quote_currency, base_min_size, base_max_size,
           base_increment, quote_increment)) ->
      { id; base_currency; quote_currency; base_min_size; base_max_size;
        base_increment; quote_increment })
    (merge_objs unit
       (obj7
          (req "symbol" string)
          (req "baseCurrency" string)
          (req "quoteCurrency" string)
          (req "baseMinSize" strfl)
          (req "baseMaxSize" strfl)
          (req "baseIncrement" strfl)
          (req "quoteIncrement" strfl)))

let book level =
  conv
    (fun _ -> assert false)
    (fun (seq, ts, bids, asks) -> { seq; ts; bids; asks })
    (obj4
       (req "sequence" strint)
       (req "time" time_ms)
       (req "bids" (list level))
       (req "asks"(list level)))

let symbols ?(sandbox=false) () =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v1/symbols" in
  Fastrest.get (or_error (list symbol)) url

let book20 ?(sandbox=false) symbol =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v1/market/orderbook/level2_20" in
  let url = Uri.with_query' url ["symbol", Pair.to_string symbol] in
  Fastrest.get (or_error (book l2)) url

let book100 ?(sandbox=false) symbol =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v1/market/orderbook/level2_100" in
  let url = Uri.with_query' url ["symbol", Pair.to_string symbol] in
  Fastrest.get (or_error (book l2)) url

let bookFull ?(sandbox=false) symbol =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v2/market/orderbook/level2" in
  let url = Uri.with_query' url ["symbol", Pair.to_string symbol] in
  Fastrest.get (or_error (book l2)) url

let bookL3 ?(sandbox=false) symbol =
  let url = Uri.with_path (if sandbox then url else sandbox_url) "api/v1/market/orderbook/level3" in
  let url = Uri.with_query' url ["symbol", Pair.to_string symbol] in
  Fastrest.get (or_error (book l3)) url
