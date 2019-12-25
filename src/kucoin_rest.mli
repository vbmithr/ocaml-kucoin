open Kucoin

type symbol = {
  id: string ;
  base_currency: string ;
  quote_currency: string ;
  base_min_size: float ;
  base_max_size: float ;
  base_increment: float ;
  quote_increment: float ;
} [@@deriving sexp]

val pair_of_symbol : symbol -> Pair.t
val symbols : ?sandbox:bool -> unit -> (Fastrest.form, symbol list) Fastrest.service

type l2 = { price: float ; size: float } [@@deriving sexp]
type l3 = { orderID: Uuidm.t ; price: float ; size: float } [@@deriving sexp]

type 'a book = {
  seq: int64 ;
  ts: Ptime.t ;
  bids: 'a list ;
  asks: 'a list ;
} [@@deriving sexp]

val book20: ?sandbox:bool -> Pair.t -> (Fastrest.form, l2 book) Fastrest.service
val book100: ?sandbox:bool -> Pair.t -> (Fastrest.form, l2 book) Fastrest.service
val bookFull: ?sandbox:bool -> Pair.t -> (Fastrest.form, l2 book) Fastrest.service
val bookL3: ?sandbox:bool -> Pair.t -> (Fastrest.form, l3 book) Fastrest.service
