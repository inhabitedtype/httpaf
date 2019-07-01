(** This struct holds curried functions bound to a handler. *)
type t = {

  (* Reading *)
  next_read_operation : unit -> [`Read | `Yield | `Close] ;

  read :
    Bigstringaf.t
    -> off:int
    -> len:int

    -> int ;

  read_eof :
    Bigstringaf.t
    -> off:int
    -> len:int
    -> int ;

  yield_reader : unit -> (unit -> unit) -> unit ;

  (* Writing *)
  next_write_operation : unit -> 
    [ `Write of Bigstringaf.t Faraday.iovec list
    | `Yield 
    | `Close of int ] ;

  report_write_result :  [`Closed | `Ok of int] -> unit ;

  yield_writer : (unit -> unit) -> unit ;

  (* Error *)
  report_exn : exn -> unit ;

  switch_handler : unit -> t option
}

type bind_t = t


module type S = sig
  type t

  (** Reading *)
  val next_read_operation : t -> [`Read | `Yield | `Close]

  val read :
    t
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> int

  val read_eof :
    t
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> int

  val yield_reader :
    t
    -> (unit -> unit)
    -> unit

  (** Writing *)

  val next_write_operation : t -> 
    [ `Write of Bigstringaf.t Faraday.iovec list
    | `Yield 
    | `Close of int]

  val report_write_result : t -> [ `Closed | `Ok of int] -> unit

  val yield_writer : t -> (unit -> unit) -> unit

  val report_exn :
    t
    -> exn
    -> unit

  val switch_handler : t -> bind_t option
end


(** Binds the handler struct to the handling functions so that the caller does not
    need to know about the handler's type. *)
let bind (type a) m handler =
  let module Handler = (val m:S with type t = a) in
  
  { next_read_operation = (fun () -> Handler.next_read_operation handler)
  ; read = Handler.read handler
  ; read_eof = Handler.read_eof handler
  ; yield_reader = (fun () -> Handler.yield_reader handler)
  ; next_write_operation = (fun () -> Handler.next_write_operation handler)
  ; report_write_result = Handler.report_write_result handler
  ; yield_writer = Handler.yield_writer handler
  ; report_exn = Handler.report_exn handler 
  ; switch_handler = (fun () -> Handler.switch_handler handler) }
