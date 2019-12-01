open Kucoin

val url : Uri.t
val sandbox_url : Uri.t

type l2book = {
  seq: int64 ;
  ts: Ptime.t ;
  bids: level list ;
  asks: level list ;
}
and level = { price: float ; size: float }
[@@deriving sexp]

val book20: ?sandbox:bool -> Pair.t -> (Fastrest.form, l2book) Fastrest.service
val book100: ?sandbox:bool -> Pair.t -> (Fastrest.form, l2book) Fastrest.service
val book: ?sandbox:bool -> Pair.t -> (Fastrest.form, l2book) Fastrest.service
