type t =
  { read_buffer_size          : int
  ; request_body_buffer_size  : int
  ; response_buffer_size      : int
  ; response_body_buffer_size : int }

let default =
  { read_buffer_size          = 0x1000
  ; request_body_buffer_size  = 0x1000
  ; response_buffer_size      = 0x400
  ; response_body_buffer_size = 0x1000 }
