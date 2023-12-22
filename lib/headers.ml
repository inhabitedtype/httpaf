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


type name = string
type value = string
type t = (name * value) list

let empty : t = []

let of_rev_list t = t
let of_list t = of_rev_list (List.rev t)
let to_rev_list t = t
let to_list t = List.rev (to_rev_list t)

module CI = struct
  (* Convert codes to upper case and compare them. This is a port of assembly
     code from the page:

      http://www.azillionmonkeys.com/qed/asmexample.html *)
  let[@inline always] char_code_equal_ci x y =
    let codes = (x lsl 8) lor y in
    let b = 0x8080 lor codes in
    let c = b - 0x6161 in
    let d = lnot (b - 0x7b7b) in
    let e = (c land d) land (lnot codes land 0x8080) in
    let upper = codes - (e lsr 2) in
    upper lsr 8 = upper land 0xff

  let equal x y =
    let len = String.length x in
    len = String.length y && (
      let equal_so_far = ref true in
      let i            = ref 0 in
      while !equal_so_far && !i < len do
        let c1 = Char.code (String.unsafe_get x !i) in
        let c2 = Char.code (String.unsafe_get y !i) in
        equal_so_far := char_code_equal_ci c1 c2;
        incr i
      done;
      !equal_so_far
    )
  ;;
end

let ci_equal = CI.equal

let rec mem t name =
  match t with
  | (name', _)::t' -> CI.equal name name' || mem t' name
  | _             -> false

let add t name value = (name,value)::t
let add_list t ls = ls @ t (* XXX(seliopou): do better here *)
let add_multi =
  let rec loop_outer t lss =
    match lss with
    | [] -> t
    | (n,vs)::lss' -> loop_inner t n vs lss'
  and loop_inner t n vs lss =
    match vs with
    | []     -> loop_outer t lss
    | v::vs' -> loop_inner ((n,v)::t) n vs' lss
  in
  loop_outer

let add_unless_exists t name value =
  if mem t name then t else (name,value)::t

exception Local

let replace t name value =
  let rec loop t needle nv seen =
    match t with
    | [] ->
      if not seen then raise Local else []
    | (name,_ as nv')::t ->
      if CI.equal needle name
      then (
        if seen
        then loop t needle nv true
        else nv::loop t needle nv true)
      else nv'::loop t needle nv seen
  in
  try loop t name (name,value) false
  with Local -> t

let remove t name =
  let rec loop s needle seen =
    match s with
    | [] ->
      if not seen then raise Local else []
    | (name,_ as nv')::s' ->
      if CI.equal needle name
      then loop s' needle true
      else nv'::(loop s' needle seen)
  in
  try loop t name false
  with Local -> t

let get t name =
  let rec loop t n =
    match t with
    | [] -> None
    | (n',v)::t' -> if CI.equal n n' then Some v else loop t' n
  in
  loop t name

let get_exn t name =
  let rec loop t n =
    match t with
    | [] -> failwith (Printf.sprintf "Headers.get_exn: %S not found" name)
    | (n',v)::t' -> if CI.equal n n' then v else loop t' n
  in
  loop t name

let get_multi t name =
  let rec loop t n acc =
    match t with
    | [] -> acc
    | (n',v)::t' ->
      if CI.equal n n'
      then loop t' n (v::acc)
      else loop t' n acc
  in
  loop t name []

let iter ~f t =
  List.iter (fun (name,value) -> f name value) t

let fold ~f ~init t =
  List.fold_left (fun acc (name,value) -> f name value acc) init t

let to_string t =
  let b = Buffer.create 128 in
  iter (to_list t) ~f:(fun name value ->
    Buffer.add_string b name;
    Buffer.add_string b ": ";
    Buffer.add_string b value;
    Buffer.add_string b "\r\n");
  Buffer.add_string b "\r\n";
  Buffer.contents b

let pp_hum fmt t =
  let pp_elem fmt (n,v) = Format.fprintf fmt "@[(%S %S)@]" n v in
  Format.fprintf fmt "@[(";
  Format.pp_print_list pp_elem fmt (to_list t);
  Format.fprintf fmt ")@]";
