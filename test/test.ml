open Core
open Async

open Kucoin
open Kucoin_rest

let wrap_request
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n service =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    (Fastrest.request ?auth:None service) |>
    Deferred.ignore_m
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 5)
    ?(speed=`Quick) n f =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.Or_error.ignore_m |>
    Deferred.Or_error.ok_exn
  end

let test_later = ref []

let rest = [
  wrap_request "symbols" (symbols ()) ;
  wrap_request "book_sandbox"
    (book20 ~sandbox:true (Pair.create ~base:"BTC" ~quote:"USDT")) ;
  wrap_request "book"
    (book20 ~sandbox:false (Pair.create ~base:"BTC" ~quote:"USDT")) ;
  wrap_request "bookL3"
    (book20 ~sandbox:false (Pair.create ~base:"BTC" ~quote:"USDT")) ;
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run ~and_exit:false "kucoin" [
    "rest", rest ;
  ] ;
  Alcotest.run "kucoin_later" !test_later
