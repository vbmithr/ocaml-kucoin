open Core
open Async
open Kucoin
open Kucoin_rest
open Alcotest_async

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  test_case ~timeout n speed begin fun () ->
    (Fastrest.request ?auth:None service) |>
    Deferred.ignore_m
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n f =
  test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.Or_error.ignore_m |>
    Deferred.Or_error.ok_exn
  end

let rest = [
  wrap_request "symbols" (symbols ()) ;
  wrap_request "book_sandbox"
    (book20 ~sandbox:true (Pair.create ~base:"BTC" ~quote:"USDT")) ;
  wrap_request "book"
    (book20 ~sandbox:false (Pair.create ~base:"BTC" ~quote:"USDT")) ;
  wrap_request "bookL3"
    (book20 ~sandbox:false (Pair.create ~base:"BTC" ~quote:"USDT")) ;
]

let main () =
  run "kucoin" [
    "rest", rest ;
  ]

let () =
  don't_wait_for (main ()) ;
  never_returns (Scheduler.go ())
