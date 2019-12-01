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
    cleaned_up: unit Deferred.t ;
  }

  let create r w cleaned_up = { r; w; cleaned_up }

  module Address = struct
    type t = bool [@@deriving sexp]
    let equal = Stdlib.(=)
  end

  let is_closed { r; _ } = Pipe.is_closed r
  let close { r; w; cleaned_up } =
    Pipe.close w ; Pipe.close_read r ; cleaned_up
  let close_finished { cleaned_up; _ } = cleaned_up
end
include T

let req sandbox =
  Uri.with_path Kucoin_rest.(if sandbox then sandbox_url else url) "api/v1/bullet-public"

let always_bullet =
  let open Json_encoding in
  conv (fun _ -> assert false) (fun a -> Ok a) Kucoin_ws.bullet

let srv sandbox =
  Fastrest.post_json
    ~params:(Json_encoding.empty, ()) always_bullet (req sandbox)

let get_url sandbox =
  Fastrest.request (srv sandbox) >>|? fun { code = _ ; servers; token } ->
  let server = List.hd_exn servers in
  Uri.with_query server.endpoint
    ["token", [token]; "acceptUserMessage", ["true"]]

let connect ?(sandbox=false) () =
  get_url sandbox >>=? fun url ->
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; cleaned_up } ->
      let client_read = Pipe.map r ~f:begin fun msg ->
          Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
        end in
      let ws_read, client_write = Pipe.create () in
      don't_wait_for
        (Pipe.closed client_write >>| fun () -> Pipe.close w) ;
      don't_wait_for @@
      Pipe.transfer ws_read w ~f:begin fun cmd ->
        let doc =
          match Ezjsonm_encoding.construct encoding cmd with
          | `A _ | `O _ as a -> Ezjsonm.to_string a
          | _ -> invalid_arg "not a json document" in
        Log.debug (fun m -> m "-> %s" doc) ;
        doc
      end ;
      create client_read client_write cleaned_up
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay () =
    create ~server_name ?on_event ?retry_delay ~connect:(fun sandbox -> connect ~sandbox ())
end

let connect_exn ?sandbox url =
  connect ?sandbox url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?(sandbox=false) f =
  get_url sandbox >>=? fun url ->
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    let client_read = Pipe.map r ~f:begin fun msg ->
        Ezjsonm_encoding.destruct_safe encoding (Ezjsonm.from_string msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      let doc =
        match Ezjsonm_encoding.construct encoding cmd with
        | `A _ | `O _ as a -> Ezjsonm.to_string a
        | _ -> invalid_arg "not a json document" in
      Log.debug (fun m -> m "-> %s" doc) ;
      doc
    end ;
    f client_read client_write
  end

let with_connection_exn ?sandbox f =
  with_connection ?sandbox f >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
