(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

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
    http/af respects the imperatives of the [Connection] header when handling
    HTTP 1.0 connections.

    To use this library effectively, the user must be familiar with the HTTP
    1.1 specification, and the basic principles of memory management and
    vectorized IO. *)


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
    | `Request_entity_too_large
    | `Request_uri_too_long
    | `Unsupported_media_type
    | `Requested_range_not_satisfiable
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
