(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

(* An experiment into how to do Functional Reactive Programming with
   Eio *)

open Eio.Std

module E : sig
  type 'a t

  val create : unit -> 'a t * ('a -> unit)
  (** [create ()] returns an event [e] and a [send] function. Calling
      the function [send] with some value [v] generates an occurence of
      [v] on [e]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t

  val merge : sw:Switch.t -> 'a t list -> 'a t
  (** [merge ~sw es] returns an event [e] that occurs on every
      occurence of all events in [es]. *)

  val to_seq : 'a t -> 'a Seq.t
  (** [to_seq e] returns a sequence carrying the future occurences of
      [e].

      TODO: explain semantics - the word "future" is probably not
      adequate.

      Consuming the returned sequence blocks the fiber until the next
      occurence of [e]. This means that the [send] function must be
      called in a different fiber.

      The returned sequence is ephermal. *)
end = struct
  type cons = Cons of (Obj.t * cons) Promise.t
  type resolver = (Obj.t * cons) Promise.u
  type 'a t = { current : (cons * resolver) Atomic.t; of_obj : Obj.t -> 'a }

  let send e v =
    (* TODO: there might be a race condition here that would cause a
       resolved promise to be resolved again (and an event to be
       dropped). *)
    let _, resolver = Atomic.get e.current in
    let next_promise, next_resolver = Promise.create () in
    Promise.resolve resolver (Obj.repr v, Cons next_promise);
    Atomic.set e.current (Cons next_promise, next_resolver)

  let create () =
    let promise, resolver = Promise.create () in

    let e =
      { current = Atomic.make (Cons promise, resolver); of_obj = Obj.magic }
    in

    (e, send e)

  let map f e = { e with of_obj = (fun obj -> f @@ e.of_obj obj) }

  let to_seq e =
    let rec loop (Cons p) () =
      let obj, next = Promise.await p in
      Seq.Cons (e.of_obj obj, loop next)
    in

    let cons, _ = Atomic.get e.current in
    loop cons

  let merge ~sw es =
    let e, send = create () in
    Fiber.fork ~sw (fun () ->
        Fiber.List.iter (fun ei -> to_seq ei |> Seq.iter send) es);
    e
end
