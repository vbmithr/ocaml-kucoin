open Sexplib.Std

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)
end

module Uuidm = struct
  include Uuidm

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_string sexp_str with
    | None -> invalid_arg "Uuidm.t_of_sexp"
    | Some u -> u

  let sexp_of_t t =
    sexp_of_string (to_string t)
end

module Pair = struct
  module T = struct
    type t = {
      base: string;
      quote: string;
    } [@@deriving sexp]

    let compare = Stdlib.compare
    let hash = Hashtbl.hash
    let equal = Stdlib.(=)
  end
  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)
  module Table = Hashtbl.Make(T)

  let create ~base ~quote = { base; quote }
  let pp ppf { base; quote } = Format.fprintf ppf "%s-%s" base quote
  let pp_list = Format.pp_print_list ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ',') pp
  let to_string { base; quote } = base ^ "-" ^ quote
  let of_string s = match String.split_on_char '-' s with
    | [base; quote ] -> { base ; quote }
    | _ -> invalid_arg "Pair.of_string"

  let encoding = Json_encoding.(conv to_string of_string string)
end

open Json_encoding

let uri = conv Uri.to_string Uri.of_string string
let strint = conv Int64.to_string Int64.of_string string
let strfl = conv Float.to_string (function "" -> nan | s -> Float.of_string s) string
let time_ns = conv
    (fun _ -> assert false)
    (fun s -> Option.get (Ptime.of_float_s (Float.of_string s /. 1e9)))
    string
let time_ms = conv
    (fun _ -> assert false)
    (fun s -> Option.get (Ptime.of_float_s (s /. 1e3)))
    float

let orderID_of_hex hex =
  let bytes = Hex.to_bytes hex in
  let buf = Bytes.make 16 '\x00' in
  Bytes.blit bytes 0 buf 4 12 ;
  let buf = Bytes.unsafe_to_string buf in
  Option.get (Uuidm.of_bytes buf)
