open Core
open Async

open Kucoin
open Kucoin_ws

let src = Logs.Src.create "kucoin.ws.async"
module Log = (val Logs.src_log src : Logs.LOG)

module T = struct
  type t = {
    r: Kucoin_ws.t Pipe.Reader.t ;
    w: Kucoin_ws.t Pipe.Writer.t ;
  }

  let create r w = { r; w }

  module Address = Uri_sexp

  let is_closed { r; w } = Pipe.(is_closed r && is_closed w)
  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit

  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r;
                       Pipe.closed w]
end
include T

let req url = Uri.with_path url "api/v1/bullet-public"

let always_bullet =
  let open Json_encoding in
  conv (fun _ -> assert false) (fun a -> Ok a) Kucoin_ws.bullet

let srv url =
  Fastrest.post_json
    ~params:(Json_encoding.empty, ()) always_bullet (req url)

let get_url url =
  Fastrest.request (srv url) >>|? fun { code = _ ; servers; token } ->
  let server = List.hd_exn servers in
  let ival = Caml.Option.get (Ptime.Span.to_int_s server.pingIval) in
  Time_ns.Span.of_int_sec ival,
  Uri.with_query server.endpoint
    ["token", [token]; "acceptUserMessage", ["true"]]

let mk_client_read r =
  Pipe.map r ~f:begin fun msg ->
    Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
  end

let mk_client_write w =
  Pipe.create_writer begin fun ws_read ->
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end
  end

let connect url =
  get_url url >>=? fun (hb, url) ->
  Deferred.Or_error.map (Fastws_async.connect ~hb url)
    ~f:begin fun { r; w; _ } ->
      let client_write = mk_client_write w in
      (Pipe.closed client_write >>> fun () -> Pipe.close w) ;
      create (mk_client_read r) client_write
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay =
    create ~server_name ?on_event ?retry_delay ~connect
end

let connect_exn url =
  connect url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ~f url =
  get_url url >>=? fun (hb, url) ->
  Fastws_async.with_connection ~hb url ~f:begin fun r w ->
    f (mk_client_read r) (mk_client_write w)
  end

let with_connection_exn ~f url =
  with_connection ~f url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
