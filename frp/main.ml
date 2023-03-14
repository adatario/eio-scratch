(*
 * SPDX-FileCopyrightText: 2023 Tarides <contact@tarides.com>
 *
 * SPDX-License-Identifier: ISC
 *)

open Eio.Std
open Frp

let rec tick clock send i =
  send i;
  Eio.Time.sleep clock 0.5;
  tick clock send (i + 1)

let main stdenv =
  let clock = Eio.Stdenv.clock stdenv in
  let e1, send = E.create () in

  let e2 = E.map (fun i -> i * 2) e1 in
  Switch.run (fun sw ->
      (* let e3 = E.merge ~sw [ e1; e2 ] in *)
      Fiber.fork ~sw (fun () ->
          E.to_seq e1 |> Seq.take 10 |> Seq.iter (fun v -> traceln "e1: %d" v));

      send 0;
      send 1;

      Fiber.fork_daemon ~sw (fun () ->
          E.to_seq e1 |> Seq.iter (fun v -> traceln "e1 (later): %d" v);
          `Stop_daemon);
      Fiber.fork_daemon ~sw (fun () ->
          E.to_seq e2 |> Seq.iter (fun v -> traceln "e2 (later): %d" v);
          `Stop_daemon);
      send 2;
      Eio.Time.sleep clock 0.5;
      send 3;
      tick clock send 0)

let () = Eio_main.run main
