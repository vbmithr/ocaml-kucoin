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
