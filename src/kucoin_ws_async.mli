open Core
open Async

type t = {
  r: Kucoin_ws.t Pipe.Reader.t ;
  w: Kucoin_ws.t Pipe.Writer.t ;
  cleaned_up: unit Deferred.t ;
}

val connect : ?sandbox:bool -> unit -> t Deferred.Or_error.t
val connect_exn : ?sandbox:bool -> unit -> t Deferred.t

val with_connection : ?sandbox:bool ->
  (Kucoin_ws.t Pipe.Reader.t ->
   Kucoin_ws.t Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn : ?sandbox:bool ->
  (Kucoin_ws.t Pipe.Reader.t ->
   Kucoin_ws.t Pipe.Writer.t -> 'a Deferred.t) -> 'a Deferred.t

module Persistent : sig
  include Persistent_connection_kernel.S
    with type address = bool
     and type conn = t

  val create' :
    server_name:string ->
    ?on_event:(Event.t -> unit Deferred.t) ->
    ?retry_delay:(unit -> Time_ns.Span.t) -> unit ->
    (unit -> address Or_error.t Deferred.t) -> t
end
