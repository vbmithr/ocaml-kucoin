open Kucoin
open Json_encoding

type bullet = {
  code: string;
  servers: server list;
  token: string;
}
and server = {
  pingIval: Ptime.Span.t;
  endpoint: Uri.t;
  encrypt: bool;
  pingTimeout: Ptime.Span.t;
}

val bullet : bullet encoding

type topic =
  | Ticker
  | Match
  | L2
  | L3

type sub = {
  id: string;
  topic: topic;
  pairs: Pair.t list;
  priv: bool;
  response: bool;
}

val l3 : Pair.t list -> sub
val l2 : Pair.t list -> sub

type err = {
  id: string ;
  code: int ;
  data: string ;
} [@@deriving sexp]

type 'a message = {
  subject: string ;
  topic: topic ;
  pairs: Pair.t list ;
  data: 'a ;
} [@@deriving sexp]

type received = {
  sequence: int64 ;
  symbol: Pair.t ;
  side: Fixtypes.Side.t ;
  ordType: Fixtypes.OrdType.t ;
  price: float option ;
  ts: Ptime.t ;
  orderID: Uuidm.t ;
  clOrdID: string option ;
} [@@deriving sexp]

type done_ = {
  sequence: int64 ;
  symbol: Pair.t ;
  side: Fixtypes.Side.t ;
  reason: Fixtypes.OrdStatus.t ;
  orderID: Uuidm.t ;
  price: float option ;
  size: float option ;
  ts: Ptime.t ;
} [@@deriving sexp]

type open_ = {
  sequence: int64 ;
  symbol: Pair.t ;
  side: Fixtypes.Side.t ;
  orderID: Uuidm.t ;
  price: float;
  size: float;
  ts: Ptime.t ;
} [@@deriving sexp]

type match_ = {
  sequence: int64 ;
  symbol: Pair.t ;
  side: Fixtypes.Side.t ;
  price: float ;
  size: float ;
  takerOrdID: Uuidm.t ;
  makerOrdID: Uuidm.t ;
  tradeID: Uuidm.t ;
  ts: Ptime.t ;
} [@@deriving sexp]

type change = {
  sequence: int64 ;
  symbol: Pair.t ;
  side: Fixtypes.Side.t ;
  orderID: Uuidm.t ;
  price: float ;
  oldSize: float ;
  newSize: float ;
  ts: Ptime.t ;
} [@@deriving sexp]

type level = {
  price: float ;
  size: float ;
  seq: int64 ;
} [@@deriving sexp]

type l2update = {
  symbol: Pair.t ;
  seqStart: int64 ;
  seqEnd: int64 ;
  bids: level list ;
  asks: level list ;
} [@@deriving sexp]

type t =
  | Ack of int64
  | Welcome of string
  | Ping of string
  | Pong of string
  | Subscribe of sub
  | Unsubscribe of sub
  | Received of received message
  | Done of done_ message
  | Open of open_ message
  | Match of match_ message
  | Change of change message
  | L2 of l2update message
  | Error of err
[@@deriving sexp]

val pp : t Fmt.t

val encoding : t encoding
