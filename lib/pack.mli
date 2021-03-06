(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(** Pack files. *)

type t = Packed_value.pic list
(** A pack value is an ordered list of position-independant packed
    values and the keys of the corresponding inflated objects. *)

val keys: t -> Hash.Set.t
(** Return the keys present in the pack. *)

include Object.S with type t := t

type raw
(** The type for raw packs. *)

module Raw: sig

  include Object.S with type t = raw

  val index: t -> Pack_index.Raw.t
  (** Get the raw index asoociated to the raw pack. *)

  val name: t -> Hash.t
  (** Get the name of the pack. *)

  val keys: t -> Hash.Set.t
  (** Get the keys present in the raw pack. *)

  val buffer: t -> Cstruct.t
  (** Get the pack buffer. *)

  val shallow: t -> bool
  (** [shallow t] is true if all the Hash references appearing in [t]
      corresponds to objects also in [t]. *)

  val input_header: Mstruct.t -> [`Version of int] * [`Count of int]
  (** [input_head buf] reads the pack [version] number (could be 2 or
      3) and the [count] of packed values in the pack file. *)

end

module type IO = sig

  include Object.S with type t = t

  val add: ?level:int -> t -> Pack_index.raw * Cstruct.t
  (** Serialize a pack file into a list of buffers. Return the
      corresponding raw index. *)

  val input: ?progress:(string -> unit) ->
    index:Pack_index.f -> keys:Hash.Set.t -> read:Value.read_inflated ->
    Mstruct.t -> t Lwt.t
  (** The usual [Object.S.input] function, but with additionals [index]
      and [keys] arguments to speed-up ramdom accesses and [read] to
      read shallow objects external to the pack file. *)

  val read: t -> Hash.t -> Value.t option
  (** Return the value stored in the pack file. *)

  val read_exn: t -> Hash.t -> Value.t
  (** Return the value stored in the pack file. *)

  val create: (Hash.t * Value.t) list -> t
  (** Create a (not very well compressed) pack file. *)

  module Raw: sig

    (** Raw pack file: they contains a pack index and a list of
        position-dependant deltas. *)

    include Object.S with type t = raw

    val add: [`Not_defined]
    (** [Pack.Raw.add] is not defined. Use {!buffer} instead. *)

    val input: ?progress:(string -> unit) -> read:Value.read_inflated ->
      Mstruct.t -> t Lwt.t
    (** [input ~read buf] is the raw pack and raw index obtained by
        reading the buffer [buf]. External (shallow) Hash1 references are
        resolved using the [read] function; these references are needed
        to unpack the list of values stored in the pack file, to then
        compute the full raw index. *)

    val unpack: ?progress:(string -> unit) -> write:Value.write_inflated ->
      t -> Hash.Set.t Lwt.t
    (** Unpack a whole pack file on disk (by calling [write] on every
        values) and return the Hash1s of the written objects. *)

    val read: index:Pack_index.f -> read:Value.read_inflated ->
      Mstruct.t -> Hash.t -> Value.t option Lwt.t
    (** Same as the top-level [read] function but for raw packs. *)

    val read_inflated: index:Pack_index.f -> read:Value.read_inflated ->
      Mstruct.t -> Hash.t -> string option Lwt.t
    (** Same as {!read} but for inflated values. *)

    val size: index:Pack_index.f -> Mstruct.t -> Hash.t -> int option Lwt.t
    (** [size ~index buf h] is the (uncompressed) size of [h]. *)
  end

  type raw = Raw.t
  (** The type for raw packs. *)

  val of_raw: ?progress:(string -> unit) -> Raw.t -> t Lwt.t
  (** Transform a raw pack file into a position-independant pack
      file. *)

  val to_raw: t -> Raw.t
    (** Transform a position-independant pack file into a raw pack and
        index files. *)

end

module IO (D: Hash.DIGEST) (I: Inflate.S): IO
