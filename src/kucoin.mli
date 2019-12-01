module Ezjsonm_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Ezjsonm)
  val destruct_safe : 'a Json_encoding.encoding -> Ezjsonm.value -> 'a
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span

  include Sexplib0.Sexpable.S with type t = Ptime.t
end

module Uuidm : sig
  include module type of Uuidm
    with type t = Uuidm.t

  include Sexplib0.Sexpable.S with type t = Uuidm.t
end

module Pair : sig
  type t = {
    base: string;
    quote: string;
  } [@@deriving sexp]

  val create : base:string -> quote:string -> t
  val equal : t -> t -> bool

  val pp : t Fmt.t
  val pp_list : t list Fmt.t

  val to_string : t -> string
  val of_string : string -> t

  val encoding : t Json_encoding.encoding
end

open Json_encoding

val uri : Uri.t encoding
val strint : int64 encoding
val strfl : float encoding

val time_ns : Ptime.t encoding
(** time encoded as a string: number of ns *)

val time_ms : Ptime.t encoding
(** time encoded as a float: number of ms *)

val orderID_of_hex : Hex.t -> Uuidm.t
