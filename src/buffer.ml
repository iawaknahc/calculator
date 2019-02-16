type t =
  { sign : float
  ; has_dot : bool
  ; appendable : bool
  ; value : string }

let to_float buf = buf.sign *. float_of_string buf.value

let format buf = buf |> to_float |> Misc.format_float

let new_number n =
  {sign = 1.0; has_dot = false; appendable = true; value = string_of_int n}


let new_sign = {sign = -1.0; has_dot = false; appendable = true; value = "0"}

let new_dot = {sign = 1.0; has_dot = true; appendable = true; value = "0."}

let new_result n =
  let sign = if n < 0.0 then -1.0 else 1.0 in
  let has_dot = floor n <> n in
  {sign; has_dot; appendable = false; value = string_of_float (abs_float n)}


let zero = {sign = 1.0; has_dot = false; appendable = true; value = "0"}

let toggle_sign buf = {buf with sign = -1.0 *. buf.sign}

let append_dot buf =
  if not buf.appendable
  then new_dot
  else if buf.has_dot
  then buf
  else {buf with has_dot = true; value = buf.value ^ "."}


let append_number buf max n =
  if n < 0 || n > 9
  then buf
  else if not buf.appendable
  then new_number n
  else
    match buf.value with
    | "0" ->
        {buf with value = string_of_int n}
    | value ->
        let len = String.length value in
        let len = if buf.has_dot then len - 1 else len in
        if len >= max then buf else {buf with value = value ^ string_of_int n}
