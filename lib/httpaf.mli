(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

(** Http/af is a high-performance, memory-efficient, and scalable web server
    for OCaml. It implements the HTTP 1.1 specification with respect to
    parsing, serialization, and connection pipelining. For compatibility,
    http/af respects the imperatives of the [Server_connection] header when handling
    HTTP 1.0 connections.

    To use this library effectively, the user must be familiar with the HTTP
    1.1 specification, and the basic principles of memory management and
    vectorized IO. *)


open Result

(** {2 Basic HTTP Types} *)


(** Protocol Version

    HTTP uses a "<major>.<minor>" numbering scheme to indicate versions of the
    protocol. The protocol version as a whole indicates the sender's conformance
    with the set of requirements laid out in that version's corresponding
    specification of HTTP.

    See {{:https://tools.ietf.org/html/rfc7230#section-2.6} RFC7230§2.6} for
    more details. *)
module Version : sig
  type t =
    { major : int (** The major protocol number. *)
    ; minor : int (** The minor protocol number. *)
    }

  val compare : t -> t -> int

  val to_string : t -> string
  val of_string : string -> t

  val pp_hum : Format.formatter -> t -> unit
end


(** Request Method

    The request method token is the primary source of request semantics;
    it indicates the purpose for which the client has made this request
    and what is expected by the client as a successful result.

    See {{:https://tools.ietf.org/html/rfc7231#section-4} RFC7231§4} for more
    detials. *)
module Method : sig
  type standard = [
    | `GET
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.1} RFC7231§4.3.1}. Safe, Cacheable. *)
    | `HEAD
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.2} RFC7231§4.3.2}. Safe, Cacheable. *)
    | `POST
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.3} RFC7231§4.3.3}. Cacheable. *)
    | `PUT
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.4} RFC7231§4.3.4}. Idempotent. *)
    | `DELETE
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.5} RFC7231§4.3.5}. Idempotent. *)
    | `CONNECT
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.6} RFC7231§4.3.6}. *)
    | `OPTIONS
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.7} RFC7231§4.3.7}. Safe.*)
    | `TRACE
    (** {{:https://tools.ietf.org/html/rfc7231#section-4.3.8} RFC7231§4.3.8}. Safe.*)
    ]

  type t = [
    | standard
    | `Other of string
    (** Methods defined outside of RFC7231, or custom methods. *)
    ]

  val is_safe : standard -> bool
  (** Request methods are considered "safe" if their defined semantics are
      essentially read-only; i.e., the client does not request, and does not
      expect, any state change on the origin server as a result of applying a
      safe method to a target resource.  Likewise, reasonable use of a safe
      method is not expected to cause any harm, loss of property, or unusual
      burden on the origin server.

      See {{:https://tools.ietf.org/html/rfc7231#section-4.2.1} RFC7231§4.2.1}
      for more details. *)

  val is_cacheable  : standard -> bool
  (**  Request methods can be defined as "cacheable" to indicate that responses
       to them are allowed to be stored for future reuse.

       See {{:https://tools.ietf.org/html/rfc7234} RFC7234} for more details. *)

  val is_idempotent : standard -> bool
  (** A request method is considered "idempotent" if the intended effect on
      the server of multiple identical requests with that method is the same as
      the effect for a single such request.  Of the request methods defined by
      this specification, PUT, DELETE, and safe request methods are idempotent.

      See {{:https://tools.ietf.org/html/rfc7231#section-4.2.2} RFC7231§4.2.2}
      for more details. *)

  val to_string : t -> string
  val of_string : string -> t

  val pp_hum : Format.formatter -> t -> unit
end


(** Response Status Codes

   The status-code element is a three-digit integer code giving the result of
   the attempt to understand and satisfy the request.

   See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for more
   details. *)
module Status : sig
  type informational = [
    | `Continue
    | `Switching_protocols
    ]
  (** The 1xx (Informational) class of status code indicates an interim
      response for communicating connection status or request progress
      prior to completing the requested action and sending a final
      response.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.2} RFC7231§6.2}
      for more details. *)

  type successful = [
    | `OK
    | `Created
    | `Accepted
    | `Non_authoritative_information
    | `No_content
    | `Reset_content
    | `Partial_content
    ]
  (** The 2xx (Successful) class of status code indicates that the client's
      request was successfully received, understood, and accepted.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.3} RFC7231§6.3}
      for more details. *)

  type redirection = [
    | `Multiple_choices
    | `Moved_permanently
    | `Found
    | `See_other
    | `Not_modified
    | `Use_proxy
    | `Temporary_redirect
    ]
  (** The 3xx (Redirection) class of status code indicates that further
      action needs to be taken by the user agent in order to fulfill the
      request.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.4} RFC7231§6.4} for
      more details. *)

  type client_error = [
    | `Bad_request
    | `Unauthorized
    | `Payment_required
    | `Forbidden
    | `Not_found
    | `Method_not_allowed
    | `Not_acceptable
    | `Proxy_authentication_required
    | `Request_timeout
    | `Conflict
    | `Gone
    | `Length_required
    | `Precondition_failed
    | `Payload_too_large
    | `Uri_too_long
    | `Unsupported_media_type
    | `Range_not_satisfiable
    | `Expectation_failed
    | `Upgrade_required
    | `I_m_a_teapot
    | `Enhance_your_calm
    ]
  (** The 4xx (Client Error) class of status code indicates that the client
      seems to have erred.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.5} RFC7231§6.5} for
      more details. *)

  type server_error = [
    | `Internal_server_error
    | `Not_implemented
    | `Bad_gateway
    | `Service_unavailable
    | `Gateway_timeout
    | `Http_version_not_supported
    ]
  (** The 5xx (Server Error) class of status code indicates that the server is
      aware that it has erred or is incapable of performing the requested
      method.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.6} RFC7231§6.6} for
      more details. *)

  type standard = [
    | informational
    | successful
    | redirection
    | client_error
    | server_error
    ]
  (** The status codes defined in the HTTP 1.1 RFCs *)

  type t = [
    | standard
    | `Code of int ]
  (** The standard codes along with support for custom codes. *)

  val default_reason_phrase : standard -> string
  (** [default_reason_phrase standard] is the example reason phrase provided
      by RFC7231 for the [standard] status code. The RFC allows servers to use
      reason phrases besides these in responses. *)

  val to_code : t -> int
  (** [to_code t] is the integer representation of [t]. *)

  val of_code : int -> t
  (** [of_code i] is the [t] representation of [i]. [of_code] raises [Failure]
      if [i] is not a positive three-digit number. *)

  val unsafe_of_code : int -> t
  (** [unsafe_of_code i] is equivalent to [of_code i], except it accepts any
      positive code, regardless of the number of digits it has. On negative
      codes, it will still raise [Failure]. *)

  val is_informational : t -> bool
  (** [is_informational t] is true iff [t] belongs to the Informational class
      of status codes. *)

  val is_successful : t -> bool
  (** [is_successful t] is true iff [t] belongs to the Successful class of
      status codes. *)

  val is_redirection : t -> bool
  (** [is_redirection t] is true iff [t] belongs to the Redirection class of
      status codes. *)

  val is_client_error : t -> bool
  (** [is_client_error t] is true iff [t] belongs to the Client Error class of
      status codes. *)

  val is_server_error : t -> bool
  (** [is_server_error t] is true iff [t] belongs to the Server Error class of
      status codes. *)

  val is_error : t -> bool
  (** [is_server_error t] is true iff [t] belongs to the Client Error or Server
      Error class of status codes. *)

  val to_string : t -> string
  val of_string : string -> t

  val pp_hum : Format.formatter -> t -> unit
end


(** Header Fields

    Each header field consists of a case-insensitive {b field name} and a {b
    field value}. The order in which header fields {i with differing field
    names} are received is not significant. However, it is good practice to
    send header fields that contain control data first so that implementations
    can decide when not to handle a message as early as possible.

    A sender MUST NOT generate multiple header fields with the same field name
    in a message unless either the entire field value for that header field is
    defined as a comma-separated list or the header field is a well-known
    exception, e.g., [Set-Cookie].

    A recipient MAY combine multiple header fields with the same field name
    into one "field-name: field-value" pair, without changing the semantics of
    the message, by appending each subsequent field value to the combined field
    value in order, separated by a comma. {i The order in which header fields
    with the same field name are received is therefore significant to the
    interpretation of the combined field value}; a proxy MUST NOT change the
    order of these field values when forwarding a message.

    {i Note.} Unless otherwise specified, all operations preserve header field
    order and all reference to equality on names is assumed to be
    case-insensitive.

    See {{:https://tools.ietf.org/html/rfc7230#section-3.2} RFC7230§3.2} for
    more details. *)
module Headers : sig
  type t

  type name = string
  (** The type of a case-insensitive header name. *)

  type value = string
  (** The type of a header value. *)

  (** {3 Constructor} *)

  val empty : t
  (** [empty] is the empty collection of header fields. *)

  val of_list : (name * value) list -> t
  (** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the intended transmission order. The following equations
      should hold:

        {ul
        {- [to_list (of_list lst) = lst] }
        {- [get (of_list [("k", "v1"); ("k", "v2")]) "k" = Some "v2"]. }} *)

  val of_rev_list : (name * value) list -> t
  (** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the {i reverse} of the intended trasmission order. The
      following equations should hold:

        {ul
        {- [to_list (of__rev_list lst) = List.rev lst] }
        {- [get (of_list [("k", "v1"); ("k", "v2")]) "k" = Some "v1"]. }} *)

  val to_list : t -> (name * value) list
  (** [to_list t] is the association list of header fields contained in [t] in
      transmission order. *)

  val to_rev_list : t -> (name * value) list
  (** [to_rev_list t] is the association list of header fields contained in [t]
      in {i reverse} transmission order. *)

  val add : t -> name -> value -> t
  (** [add t name value] is a collection of header fields that is the same as
      [t] except with [(name, value)] added at the end of the trasmission order.
      The following equations should hold:

        {ul
        {- [get (add t name value) name = Some value] }} *)

  val add_unless_exists : t -> name -> value -> t
  (** [add_unless_exists t name value] is a collection of header fields that is
      the same as [t] if [t] already inclues [name], and otherwise is
      equivalent to [add t name value]. *)

  val add_list : t -> (name * value) list -> t
  (** [add_list t assoc] is a collection of header fields that is the same as
      [t] except with all the header fields in [assoc] added to the end of the
      transmission order, in reverse order. *)

  val add_multi : t -> (name * value list) list -> t
  (** [add_multi t assoc] is the same as

      {[
        add_list t (List.concat_map assoc ~f:(fun (name, values) ->
          List.map values ~f:(fun value -> (name, value))))
      ]}

      but is implemented more efficiently. For example,

      {[
        add_multi t ["name1", ["x", "y"]; "name2", ["p", "q"]]
          = add_list ["name1", "x"; "name1", "y"; "name2", "p"; "name2", "q"]
      ]} *)

  val remove : t -> name -> t
  (** [remove t name] is a collection of header fields that contains all the
      header fields of [t] except those that have a header-field name that are
      equal to [name]. If [t] contains multiple header fields whose name is
      [name], they will all be removed. *)

  val replace : t -> name -> value -> t
  (** [replace t name value] is a collection of header fields that is the same
      as [t] except with all header fields with a name equal to [name] removed
      and replaced with a single header field whose name is [name] and whose
      value is [value]. This new header field will appear in the transmission
      order where the first occurrence of a header field with a name matching
      [name] was found.

      If no header field with a name equal to [name] is present in [t], then
      the result is simply [t], unchanged. *)

  (** {3 Destructors} *)

  val mem : t -> name -> bool
  (** [mem t name] is true iff [t] includes a header field with a name that is
      equal to [name]. *)

  val get : t -> name -> value option
  (** [get t name] returns the last header from [t] with name [name], or [None]
      if no such header is present. *)

  val get_exn : t -> name -> value
  (** [get t name] returns the last header from [t] with name [name], or raises
      if no such header is present. *)

  val get_multi : t -> name -> value list
  (** [get_multi t name] is the list of header values in [t] whose names are
      equal to [name]. The returned list is in transmission order. *)

  (** {3 Iteration} *)

  val iter : f:(name -> value -> unit) -> t -> unit
  val fold : f:(name -> value -> 'a -> 'a) -> init:'a -> t -> 'a

  (** {3 Utilities} *)

  val to_string : t -> string

  val pp_hum : Format.formatter -> t -> unit
end

(** {2 Message Body} *)

module Body : sig
  type 'rw t

  val schedule_read
    :  [`read] t
    -> on_eof  : (unit -> unit)
    -> on_read : (Bigstring.t -> off:int -> len:int -> unit)
    -> unit
  (* [schedule_read t ~on_eof ~on_read] will setup [on_read] and [on_eof] as
     callbacks for when bytes are available in [t] for the application to
     consum, or when the input channel has been closed and no further bytes
     will be received by the application.

     Once either of these callbacks have been called, they become inactive. The
     application is responsible for scheduling subsequent reads, either within
     the [on_read] callback or by some other mechanism. *)

  val write_char : [`write] t -> char -> unit
  (** [write_char w char] copies [char] into an internal buffer. If possible,
      this write will be combined with previous and/or subsequent writes before
      transmission. *)

  val write_string : [`write] t -> ?off:int -> ?len:int -> string -> unit
  (** [write_string w ?off ?len str] copies [str] into an internal buffer. If
      possible, this write will be combined with previous and/or subsequent
      writes before transmission. *)

  val write_bigstring : [`write] t -> ?off:int -> ?len:int -> Bigstring.t -> unit
  (** [write_bigstring w ?off ?len bs] copies [bs] into an internal buffer. If
      possible, this write will be combined with previous and/or subsequent
      writes before transmission. *)

  val schedule_bigstring : [`write] t -> ?off:int -> ?len:int -> Bigstring.t -> unit
  (** [schedule_bigstring w ?off ?len bs] schedules [bs] to be
      transmitted at the next opportunity without performing a copy. [bs]
      should not be modified until a subsequent call to {!flush} has
      successfully completed. *)

  val flush : [`write] t -> (unit -> unit) -> unit
  (** [flush t f] makes all bytes in [t] available for writing to the awaiting
      output channel. Once those bytes have reached that output channel, [f]
      will be called.

      The type of the output channel is runtime-dependent, as are guarantees about
      whether those packets have been queued for deliver or have actually been
      received by the intended recipient. *)

  val close_reader : [`read] t -> unit
  (** [close_reader t] closes [t], indicating that any subsequent input
      received should be discarded. *)

  val close_writer : [`write] t -> unit
  (** [close_writer t] closes [t], causing subsequent write calls to raise. If
      [t] is writable, this will cause any pending output to become available
      to the output channel. *)

  val is_closed : _ t -> bool
  (** [is_closed t] is true if {!close} has been called on [t] and [false]
      otherwise. A closed [t] may still have pending output. *)

end


(** {2 Message Types} *)

(** Request

    A client-initiated HTTP message. *)
module Request : sig
  type t =
    { meth    : Method.t
    ; target  : string
    ; version : Version.t
    ; headers : Headers.t }

  val create
    :  ?version:Version.t (** default is HTTP 1.1 *)
    -> ?headers:Headers.t (** default is {!Headers.empty} *)
    -> Method.t
    -> string
    -> t

  val body_length : t -> [
    | `Fixed of Int64.t
    | `Chunked
    | `Error of [`Bad_request]
  ]
  (** [body_length t] is the length of the message body accompanying [t]. It is
      an error to generate a request with a close-delimited message body.

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. *)

  val persistent_connection : ?proxy:bool -> t -> bool
  (** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true].

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3 for
      more details. *)

  val pp_hum : Format.formatter -> t -> unit
end


(** Response

    A server-generated message to a {Request}. *)
module Response : sig
  type t =
    { version : Version.t
    ; status  : Status.t
    ; reason  : string
    ; headers : Headers.t }

  val create
    :  ?reason:string     (** default is determined by {!Status.default_reason_phrase} *)
    -> ?version:Version.t (** default is HTTP 1.1 *)
    -> ?headers:Headers.t (** default is {!Headers.empty} *)
    -> Status.t
    -> t
  (** [create ?reason ?version ?headers status] creates an HTTP response with
      the given parameters. For typical use cases, it's sufficient to provide
      values for [headers] and [status]. *)

  val body_length : ?proxy:bool -> request_method:Method.standard -> t -> [
    | `Fixed of Int64.t
    | `Chunked
    | `Close_delimited
    | `Error of [ `Bad_gateway | `Internal_server_error ]
  ]
  (** [body_length ?proxy ~request_method t] is the length of the message body
      accompanying [t] assuming it is a response to a request whose method was
      [request_method]. If the calling code is acting as a proxy, it should
      pass [~proxy:true]. This optional parameter only affects error reporting.

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. *)

  val persistent_connection : ?proxy:bool -> t -> bool
  (** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true].

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3 for
      more details. *)

  val pp_hum : Format.formatter -> t -> unit
end


(** Bigstring

    A block of memory allocated on the C heap. Bigstring payloads won't get
    relocated by the OCaml GC, making it safe to use in blocking system calls
    without holding the OCaml runtime lock. *)
module Bigstring : sig
  type t =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  (** For compatiblity with other libraries, [Bigstring.t] is not abstract. *)

  val create : int -> t
  (** [create len] allocates a bigstring of length [len]. *)

  val of_string : ?off:int -> ?len:int -> string -> t
  (** [of_string ?off ?len str] allocates a bigstring and copies the contents
      of [str] into it. if [off] or [len] are provided, [t] will only have
      length [len] and only the specified range of the string will be copied
      into it. *)

  val length : t -> int
  (** [length t] returns the length of the bigstring. *)

  val get        : t -> int -> char
  val unsafe_get : t -> int -> char
  (** [get        t n] returns the nth byte of [t] as a [char].
      [unsafe_get t n] does the same but will not perform bounds checking. *)

  val set        : t -> int -> char -> unit
  val unsafe_set : t -> int -> char -> unit
  (** [set        t n] returns the nth byte of [t] as a [char].
      [unsafe_set t n] does the same but will not perform bounds checking. *)

  val sub : off:int -> ?len:int -> t -> t
  (** [sub ~off ?len t] returns a sub-view into the bigstring [t], specified by
      [off] and [len]. This is a {i non-copying operation}: [t] and the
      returned sub-view will share underlying bytes. Modifying one will modify
      the other. *)

  val blit : t -> int -> t -> int -> int -> unit
  val blit_from_string : string  -> int -> t -> int -> int -> unit
  val blit_from_bytes  : Bytes.t -> int -> t -> int -> int -> unit

  val to_string : ?off:int -> ?len:int -> t -> string
end


(** IOVec *)
module IOVec : sig
  type 'a t = 'a Faraday.iovec =
    { buffer : 'a
    ; off : int
    ; len : int }

  val length  : _ t -> int
  val lengthv : _ t list -> int

  val shift  : 'a t -> int -> 'a t
  val shiftv : 'a t list -> int -> 'a t list

  val pp_hum : Format.formatter -> _ t -> unit
end

(** {2 Request Descriptor} *)
module Reqd : sig
  type 'handle t

  val request : _ t -> Request.t
  val request_body : _ t -> [`read] Body.t

  val response : _ t -> Response.t option
  val response_exn : _ t -> Response.t

  (** Responding

      The following functions will initiate a response for the corresponding
      request in [t]. Depending on the state of the current connection, and the
      header values of the response, this may cause the connection to close or
      to perisist for reuse by the client.

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3} for
      more details. *)

  val respond_with_string    : _ t -> Response.t -> string -> unit
  val respond_with_bigstring : _ t -> Response.t -> Bigstring.t -> unit
  val respond_with_streaming : _ t -> Response.t -> [`write] Body.t

  (** Exception Handling *)

  val report_exn : _ t -> exn -> unit
  val try_with : _ t -> (unit -> unit) -> (unit, exn) result
end


(** {2 Server Connection} *)

module Server_connection : sig
  module Config : sig
    type t =
      { response_buffer_size      : int (** Default is [1024] *)
      ; response_body_buffer_size : int (** Default is [4096] *)
      }

    val default : t
    (** [default] is a configuration record with all parameters set to their
        default values. *)
  end

  type 'handle t

  type error =
    [ `Bad_request | `Bad_gateway | `Internal_server_error | `Exn of exn ]

  type 'handle request_handler = 'handle Reqd.t -> unit

  type error_handler =
    ?request:Request.t -> error -> (Headers.t -> [`write] Body.t) -> unit

  val create
    :  ?config:Config.t
    -> ?error_handler:error_handler
    -> 'handle request_handler
    -> 'handle t
  (** [create ?config ?error_handler ~request_handler] creates a connection
      handler that will service individual requests with [request_handler]. *)

  val next_read_operation : _ t -> [ `Read | `Yield | `Close ]
  (** [next_read_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val read : _ t -> Bigstring.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the
      connection.  {!read} should be called after {!next_read_operation}
      returns a [`Read] value and additional input is available for the
      connection to consume. *)

  val yield_reader : _ t -> (unit -> unit) -> unit
  (** [yield_reader t continue] registers with the connection to call
      [continue] when reading should resume. {!yield_reader} should be called
      after {next_read_operation} returns a [`Yield] value. *)

  val shutdown_reader : _ t -> unit
  (** [shutdown_reader t] shutds own the read processor for the connection. All
      subsequent calls to {!next_read_operations} will return [`Close].
      {!shutdown_reader} should be called after {!next_read_operation} returns
      a [`Read] value and there is no further input available for the
      connection to consume. *)

  val next_write_operation : _ t -> [
    | `Write of Bigstring.t IOVec.t list
    | `Yield
    | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : _ t -> [`Ok of int | `Closed] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {report_write_result} should be called after a
      call to {next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} *)

  val yield_writer : _ t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {next_write_operation} returns a [`Yield] value. *)

  val report_exn : _ t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and
      that it has been attributed to [t]. Calling this function will switch [t]
      into an error state. Depending on the state [t] is transitioning from, it
      may call its error handler before terminating the connection. *)

  val is_closed : _ t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will do the same will return a
      [`Write _] until all buffered output has been flushed. *)

  val error_code : _ t -> error option
  (** [error_code t] returns the [error_code] that caused the connection to
      close, if one exists. *)

  (**/**)
  val shutdown : _ t -> unit
  (**/**)
end

(** {2 Client Connection} *)

module Client_connection : sig

  type t

  type error =
    [ `Malformed_response of string | `Invalid_response_body_length of Response.t | `Exn of exn ]

  type response_handler = Response.t -> [`read] Body.t  -> unit

  type error_handler = error -> unit

  val request
    :  Request.t
    -> error_handler:error_handler
    -> response_handler:response_handler
    -> [`write] Body.t * t

  val next_read_operation : t -> [ `Read | `Close ]
  (** [next_read_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstring.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the
      connection.  {!read} should be called after {!next_read_operation}
      returns a [`Read] value and additional input is available for the
      connection to consume. *)

  val shutdown_reader : t -> unit
  (** [shutdown_reader t] shutds own the read processor for the connection. All
      subsequent calls to {!next_read_operations} will return [`Close].
      {!shutdown_reader} should be called after {!next_read_operation} returns
      a [`Read] value and there is no further input available for the
      connection to consume. *)

  val next_write_operation : t -> [
    | `Write of Bigstring.t IOVec.t list
    | `Yield
    | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [`Ok of int | `Closed] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {report_write_result} should be called after a
      call to {next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} *)

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {next_write_operation} returns a [`Yield] value. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and
      that it has been attributed to [t]. Calling this function will swithc [t]
      into an error state. Depending on the state [t] is transitioning from, it
      may call its error handler before terminating the connection. *)

  val is_closed : t -> bool

  (**/**)
  val shutdown : t -> unit
  (**/**)
end

(**/**)

module Httpaf_private : sig
  module Serialize : sig
    val write_request  : Faraday.t -> Request.t  -> unit
    val write_response : Faraday.t -> Response.t -> unit
  end
end
