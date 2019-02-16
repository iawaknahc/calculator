let format_float f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero ->
      Printf.sprintf "%g" f
  | FP_infinite | FP_nan ->
      "Error"
