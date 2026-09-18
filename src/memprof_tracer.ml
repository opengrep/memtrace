module type Memprof_sig = sig
  include module type of Stdlib.Gc.Memprof
end

type t =
  { failed : bool Atomic.t;
    stopped : bool Atomic.t;
    mutex : Mutex.t;
    stop_memprof : unit -> unit;
    report_exn : exn -> unit;
    trace : Trace.Writer.t;
    ext_sampler : Geometric_sampler.t; }

let curr_active_tracer : t option Atomic.t  = Atomic.make None

let active_tracer () = Atomic.get curr_active_tracer

let bytes_before_ext_sample = Atomic.make max_int

let draw_sampler_bytes t =
  Geometric_sampler.draw t.ext_sampler * (Sys.word_size / 8)

let[@inline never] lock_tracer s =
  if Atomic.get s.failed then
    false
    (* During external allocations or closing, a thread may try to obtain
       a lock it already holds. In that case, Mutex.lock will throw
       an error and we can ignore it. *)
  else begin
    try
      Mutex.lock s.mutex;
      (* The failed flag can be set while we wait for the mutex, so test it
         again once we hold the lock. *)
      if Atomic.get s.failed then (Mutex.unlock s.mutex; false) else true
    with
      | Sys_error _ -> false
  end

let[@inline never] unlock_tracer s =
  assert (not (Atomic.get s.failed));
  Mutex.unlock s.mutex

let[@inline never] mark_failed s e =
  if (Atomic.compare_and_set s.failed false true) then
    s.report_exn e;
  Mutex.unlock s.mutex

let default_report_exn e =
  match e with
  | Trace.Writer.Pid_changed ->
     (* This error is silently ignored, so that if Memtrace is active across
        Unix.fork () then the child process silently stops tracing *)
     ()
  | e ->
     let msg = Printf.sprintf "Memtrace failure: %s\n" (Printexc.to_string e) in
     output_string stderr msg;
     Printexc.print_backtrace stderr;
     flush stderr

let default_memprof = (module Stdlib.Gc.Memprof : Memprof_sig)

let start ?(report_exn=default_report_exn) ?(memprof = default_memprof)
      ~sampling_rate trace =
  let (module Memprof : Memprof_sig) = memprof in
  let ext_sampler = Geometric_sampler.make ~sampling_rate () in
  let mutex = Mutex.create () in
  let profile : Memprof.t option ref = ref None in
  let s = { trace; mutex; stopped = Atomic.make false; failed = Atomic.make false;
            stop_memprof = (fun () -> Memprof.stop (); Option.iter Memprof.discard !profile);
            report_exn; ext_sampler } in
  let tracker : (_,_) Gc.Memprof.tracker = {
    alloc_minor = (fun info ->
      if lock_tracer s then begin
        match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                ~length:info.size
                ~nsamples:info.n_samples
                ~source:Minor
                ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e ->
           mark_failed s e;
           None
        end
      else None);
    alloc_major = (fun info ->
      if lock_tracer s then begin
        match Trace.Writer.put_alloc_with_raw_backtrace trace (Trace.Timestamp.now ())
                ~length:info.size
                ~nsamples:info.n_samples
                ~source:Major
                ~callstack:info.callstack
        with
        | r -> unlock_tracer s; Some r
        | exception e -> mark_failed s e; None
      end else None);
    promote = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_promote trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s; Some id
        | exception e -> mark_failed s e; None
      else None);
    dealloc_minor = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed s e);
    dealloc_major = (fun id ->
      if lock_tracer s then
        match Trace.Writer.put_collect trace (Trace.Timestamp.now ()) id with
        | () -> unlock_tracer s
        | exception e -> mark_failed s e) } in
  Atomic.set curr_active_tracer (Some s);
  Atomic.set bytes_before_ext_sample (draw_sampler_bytes s);
  profile := Some (Memprof.start ~sampling_rate ~callstack_size:max_int tracker);
  s

let stop s =
  (* Call stop to stop sampling on the current profile.
     Promotion and deallocation callbacks from a profile may run
     after stop is called, however we ignore these callbacks when
     stopping.
   *)
  if (Atomic.compare_and_set s.stopped false true) then begin
    s.stop_memprof ();
    if lock_tracer s then begin
      try Trace.Writer.close s.trace with e ->
        (Atomic.set s.failed true; s.report_exn e);
      Mutex.unlock s.mutex
    end;
    Atomic.set curr_active_tracer None
  end

let[@inline never] ext_alloc_slowpath ~bytes =
  match Atomic.get curr_active_tracer with
  | None -> Atomic.set bytes_before_ext_sample max_int; None
  | Some s ->
    if lock_tracer s then begin
      match
        let bytes_per_word = Sys.word_size / 8 in
        (* round up to an integer number of words *)
        let size_words = (bytes + bytes_per_word - 1) / bytes_per_word in
        let samples = ref 0 in
        while Atomic.get bytes_before_ext_sample <= 0 do
          ignore (Atomic.fetch_and_add bytes_before_ext_sample (draw_sampler_bytes s));
          incr samples
        done;
        assert (!samples > 0);
        let callstack = Printexc.get_callstack max_int in
        Some (Trace.Writer.put_alloc_with_raw_backtrace s.trace
                (Trace.Timestamp.now ())
                ~length:size_words
                ~nsamples:!samples
                ~source:External
                ~callstack)
      with
      | r -> unlock_tracer s; r
      | exception e -> mark_failed s e; None
    end else None

type ext_token = Trace.Obj_id.t

let ext_alloc ~bytes =
  let n = Atomic.fetch_and_add bytes_before_ext_sample (- bytes) - bytes in
  if n <= 0 then ext_alloc_slowpath ~bytes else None

let ext_free id =
  match Atomic.get curr_active_tracer with
  | None -> ()
  | Some s ->
    if lock_tracer s then begin
      match
        Trace.Writer.put_collect s.trace (Trace.Timestamp.now ()) id
      with
      | () -> unlock_tracer s; ()
      | exception e -> mark_failed s e; ()
    end
