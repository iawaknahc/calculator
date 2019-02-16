include Stdlib.String

let to_char_list s = s |> Stdlib.String.to_seq |> Stdlib.List.of_seq
