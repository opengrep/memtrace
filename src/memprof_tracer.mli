module type Memprof_sig = sig
  include module type of Stdlib.Gc.Memprof
end

type t

val default_memprof : (module Memprof_sig)

val start :
  ?report_exn:(exn -> unit) ->
  ?memprof:(module Memprof_sig) ->
  sampling_rate:float ->
  Trace.Writer.t ->
  t
val stop : t -> unit

val active_tracer : unit -> t option


type ext_token [@@immediate]
val ext_alloc : bytes:int -> ext_token option
val ext_free : ext_token -> unit
