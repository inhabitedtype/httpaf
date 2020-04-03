type t = unit -> unit

let none = Sys.opaque_identity (fun () -> ())
let some f =
  if f == none
  then failwith "Optional_thunk: this function is not representable as a some value";
  f

let is_none t = t == none
let is_some t = not (is_none t)
let call_if_some t = t ()
let unchecked_value t = t
