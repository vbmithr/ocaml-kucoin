open Sexplib.Std
open Json_encoding
open Kucoin

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

let msSpan =
  conv
    (fun t -> let _, ps = Ptime.Span.to_d_ps t in Int64.(to_int (div ps 1_000_000_000L)))
    (fun i -> Ptime.Span.v (0, Int64.(mul (of_int i) 1_000_000_000L)))
    int

let server =
  conv
    (fun _ -> assert false)
    (fun ((), pingIval, pingTimeout, endpoint, encrypt) -> { pingIval; endpoint; encrypt; pingTimeout })
    (obj5
       (req "protocol" (constant "websocket"))
       (req "pingInterval" msSpan)
       (req "pingTimeout" msSpan)
       (req "endpoint" uri)
       (req "encrypt" bool))

let data =
  obj2
    (req "instanceServers" (list server))
    (req "token" string)

let bullet =
  conv
    (fun _ -> assert false)
    (fun (code, (servers, token)) -> { code; servers; token })
    (obj2
       (req "code" string)
       (req "data" data))

type topic =
  | Ticker
  | Match
  | L2
  | L3
[@@deriving sexp]

let pp_topic ppf = function
  | Ticker -> Format.pp_print_string ppf "/market/ticker"
  | Match -> Format.pp_print_string ppf "/market/match"
  | L2 -> Format.pp_print_string ppf "/market/level2"
  | L3 -> Format.pp_print_string ppf "/market/level3"

let topic_of_string = function
  | "/market/ticker" -> Ticker
  | "/market/match" -> Match
  | "/market/level2" -> L2
  | "/market/level3" -> L3
  | _ -> invalid_arg "topic_of_string"

let pp_topic_pairs ppf (topic, pairs) =
  Format.fprintf ppf "%a:%a" pp_topic topic Pair.pp_list pairs

let string_of_topic_pairs a =
  Format.asprintf "%a" pp_topic_pairs a

let topic_pairs_of_string s =
  match String.split_on_char ':' s with
  | [topic; pairs] ->
    topic_of_string topic,
    List.map Pair.of_string (String.split_on_char ',' pairs)
  | _ -> invalid_arg "topic_pairs_of_string"

type sub = {
  id: string;
  topic: topic;
  pairs: Pair.t list;
  priv: bool;
  response: bool;
} [@@deriving sexp]

let l3 pairs =
  let id = Uuidm.(create `V4 |> to_string) in
  { id; topic = L3; pairs; priv = false; response = false }

let l2 pairs =
  let id = Uuidm.(create `V4 |> to_string) in
  { id; topic = L2; pairs; priv = false; response = false }

let sub_encoding =
  conv
    (fun { id; topic; pairs; priv; response } ->
       id, (string_of_topic_pairs (topic, pairs)),  priv, response)
    (fun _ -> assert false)
    (obj4
       (req "id" string)
       (req "topic" string)
       (req "privateChannel" bool)
       (req "response" bool))

type err = {
  id: string ;
  code: int ;
  data: string ;
} [@@deriving sexp]

let err =
  conv
    (fun _ -> assert false)
    (fun ((),code,data,id) -> { id; code; data })
    (obj4
       (req "type" (constant "error"))
       (req "code" int)
       (req "data" string)
       (req "id" string))

type 'a message = {
  subject: string ;
  topic: topic ;
  pairs: Pair.t list ;
  data: 'a ;
} [@@deriving sexp]

let message subject a =
  conv
    (fun _ -> assert false)
    (fun ((), (), topic, data) ->
       let topic, pairs = topic_pairs_of_string topic in
       { subject; topic; pairs; data })
    (obj4
       (req "type" (constant "message"))
       (req "subject" (constant subject))
       (req "topic" string)
       (req "data" a))

let side =
  conv
    (fun _ -> assert false)
    (function "buy" -> Fixtypes.Side.Buy | _ -> Fixtypes.Side.Sell)
    string

let strint = conv Int64.to_string Int64.of_string string
let strfl = conv Float.to_string (function "" -> nan | s -> Float.of_string s) string
let time_ns = conv
    (fun _ -> assert false)
    (fun s -> Option.get (Ptime.of_float_s (Float.of_string s /. 1e9)))
    string

let sss =
  obj3
    (req "sequence" strint)
    (req "symbol" Pair.encoding)
    (req "side" side)

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

let level =
  conv
    (fun _ -> assert false)
    (fun (price, size, seq) -> { price; size; seq })
    (tup3 strfl strfl strint)

type l2update = {
  symbol: Pair.t ;
  seqStart: int64 ;
  seqEnd: int64 ;
  bids: level list ;
  asks: level list ;
} [@@deriving sexp]

let l2update =
  conv
    (fun _ -> assert false)
    (fun (symbol, seqStart,seqEnd,(asks, bids)) -> { symbol ; seqStart; seqEnd ; asks ; bids })
    (obj4
       (req "symbol" Pair.encoding)
       (req "sequenceStart" int53)
       (req "sequenceEnd" int53)
       (req "changes" (obj2 (req "asks" (list level)) (req "bids" (list level)))))

let orderType =
  string_enum [
    "limit", Fixtypes.OrdType.Limit ;
    "market", Market ;
  ]

let ordStatus =
  string_enum [
    "filled", Fixtypes.OrdStatus.Filled ;
    "canceled", Fixtypes.OrdStatus.Canceled ;
  ]

let orderID_of_hex hex =
  let bytes = Hex.to_bytes hex in
  let buf = Bytes.make 16 '\x00' in
  Bytes.blit bytes 0 buf 4 12 ;
  let buf = Bytes.unsafe_to_string buf in
  Option.get (Uuidm.of_bytes buf)

let received =
  conv
    (fun _ -> assert false)
    (fun ((sequence, symbol, side),((), ordType, orderID, clOrdID, price, ts)) ->
       let orderID = orderID_of_hex (`Hex orderID) in
       { sequence; symbol; side; ordType; orderID; clOrdID; price; ts })
    ((merge_objs sss)
       (obj6
          (req "type" (constant "received"))
          (req "orderType" orderType)
          (req "orderId" string)
          (opt "clientOid" string)
          (opt "price" strfl)
          (req "time" time_ns)))

let done_ =
  conv
    (fun _ -> assert false)
    (fun ((sequence, symbol, side),((), reason, orderID, price, size, ts)) ->
       let orderID = orderID_of_hex (`Hex orderID) in
       { sequence; symbol; side; reason; orderID; price; size; ts })
    ((merge_objs sss)
       (obj6
          (req "type" (constant "done"))
          (req "reason" ordStatus)
          (req "orderId" string)
          (opt "price" strfl)
          (opt "size" strfl)
          (req "time" time_ns)))

let open_ =
  conv
    (fun _ -> assert false)
    (fun ((sequence, symbol, side),((), orderID, price, size, ts)) ->
       let orderID = orderID_of_hex (`Hex orderID) in
       { sequence; symbol; side; orderID; price; size; ts })
    ((merge_objs sss)
       (obj5
          (req "type" (constant "open"))
          (req "orderId" string)
          (req "price" strfl)
          (req "size" strfl)
          (req "time" time_ns)))

let match_ =
  conv
    (fun _ -> assert false)
    (fun ((sequence, symbol, side),((), price, size, takerOrdID, makerOrdID, tradeID, ts)) ->
       let takerOrdID = orderID_of_hex (`Hex takerOrdID) in
       let makerOrdID = orderID_of_hex (`Hex makerOrdID) in
       let tradeID = orderID_of_hex (`Hex tradeID) in
       { sequence; symbol; side; takerOrdID; makerOrdID; tradeID; price; size; ts })
    ((merge_objs sss)
       (obj7
          (req "type" (constant "match"))
          (req "price" strfl)
          (req "size" strfl)
          (req "takerOrderId" string)
          (req "makerOrderId" string)
          (req "tradeId" string)
          (req "time" time_ns)))

let change =
  conv
    (fun _ -> assert false)
    (fun ((sequence, symbol, side),((), orderID, price, oldSize, newSize, ts)) ->
       let orderID = orderID_of_hex (`Hex orderID) in
       { sequence; symbol; side; orderID; price; oldSize; newSize; ts })
    ((merge_objs sss)
       (obj6
          (req "type" (constant "change"))
          (req "orderId" string)
          (req "price" strfl)
          (req "oldSize" strfl)
          (req "newSize" strfl)
          (req "time" time_ns)))

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

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

let idstr_encoding =
  obj1 (req "id" string)

let id53_encoding =
  obj1 (req "id" int53)

let welcome =
  conv (fun id -> id, ()) (fun (id,()) -> id)
  (merge_objs idstr_encoding (obj1 (req "type" (constant "welcome"))))

let ack =
  conv (fun id -> id, ()) (fun (id,()) -> id)
  (merge_objs id53_encoding (obj1 (req "type" (constant "ack"))))

let ping =
  conv (fun id -> id, ()) (fun (id,()) -> id)
  (merge_objs idstr_encoding (obj1 (req "type" (constant "ping"))))

let pong =
  conv (fun id -> id, ()) (fun (id,()) -> id)
  (merge_objs idstr_encoding (obj1 (req "type" (constant "pong"))))

let sub =
  conv (fun id -> id, ()) (fun (id,()) -> id)
    (merge_objs sub_encoding (obj1 (req "type" (constant "subscribe"))))

let unsub =
  conv (fun id -> id, ()) (fun (id,()) -> id)
    (merge_objs sub_encoding (obj1 (req "type" (constant "unsubscribe"))))

let encoding =
  union [
    case welcome (function Welcome i -> Some i | _ -> None) (fun i -> Welcome i) ;
    case ack (function Ack i -> Some i | _ -> None) (fun i -> Ack i) ;
    case ping (function Ping i -> Some i | _ -> None) (fun i -> Ping i) ;
    case pong (function Pong i -> Some i | _ -> None) (fun i -> Pong i) ;
    case sub (function Subscribe i -> Some i | _ -> None) (fun i -> Subscribe i) ;
    case unsub (function Unsubscribe i -> Some i | _ -> None) (fun i -> Unsubscribe i) ;
    case err (function Error i -> Some i | _ -> None) (fun i -> Error i) ;
    case (message "trade.l3received" received) (function Received i -> Some i | _ -> None) (fun i -> Received i) ;
    case (message "trade.l3done" done_) (function Done i -> Some i | _ -> None) (fun i -> Done i) ;
    case (message "trade.l3open" open_) (function Open i -> Some i | _ -> None) (fun i -> Open i) ;
    case (message "trade.l3match" match_) (function Match i -> Some i | _ -> None) (fun i -> Match i) ;
    case (message "trade.l3change" change) (function Change i -> Some i | _ -> None) (fun i -> Change i) ;
    case (message "trade.l2update" l2update) (function L2 i -> Some i | _ -> None) (fun i -> L2 i) ;
  ]
