include Stdlib.List

(* TODO: Remove this as OCaml 4.08 should have List.filter_map *)
let filter_map f lst =
  Stdlib.List.to_seq lst |> Seq.filter_map f |> Stdlib.List.of_seq
