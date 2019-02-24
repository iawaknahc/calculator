let eval = Calculator.eval

let%test _ = eval "" = "0"

let%test _ = eval "1" = "1"

let%test _ = eval "12" = "12"

let%test _ = eval "12+" = "12"

let%test _ = eval "12+3" = "3"

let%test _ = eval "12+3*" = "3"

let%test _ = eval "12+3*5" = "5"

let%test _ = eval "12+3*5/" = "15"

let%test _ = eval "12+3*5/+" = "27"

let%test _ = eval "12+3*5/+=" = "54"

(* eq *)
let%test _ = eval "=" = "0"

let%test _ = eval "==" = "0"

let%test _ = eval "==1" = "1"

let%test _ = eval "==1==" = "1"

let%test _ = eval "==1==+" = "1"

let%test _ = eval "==1==+=" = "2"

let%test _ = eval "==1==+==" = "3"

let%test _ = eval "==1==+==6" = "6"

let%test _ = eval "==1==+==6=" = "7"

(* C *)
let%test _ = eval "1+2C3=" = "4"

let%test _ = eval "1+2C3==" = "7"

let%test _ = eval "1+2+C=" = "3"

let%test _ = eval "1+2+C==" = "3"

let%test _ = eval "1+2+C.2=" = "3.2"

let%test _ = eval "1+2+C.2==" = "3.4"

let%test _ = eval "1+2+C.2==~" = "-3.4"

let%test _ = eval "1+2+C.2==~=" = "-3.2"

(* Error *)
let%test _ = eval "1/0" = "0"

let%test _ = eval "1/0+" = "Error"

let%test _ = eval "1/0+*" = "Error"

let%test _ = eval "1/0+*=" = "Error"

let%test _ = eval "1/0+*=C" = "0"

let%test _ = eval "1/0+*=C12" = "12"

let%test _ = eval "1/0+*=C12+3=" = "15"

(* Percent *)
(* stack length = 0 *)
let%test _ = eval "%" = "0"

let%test _ = eval "%=" = "0"

(* stack length = 1 *)
let%test _ = eval "2%" = "0.02"

let%test _ = eval "2%=" = "0.02"

(* stack length = 2 *)
let%test _ = eval "2*%" = "0.02"

let%test _ = eval "2*%=" = "0.04"

let%test _ = eval "2/%" = "0.02"

let%test _ = eval "2/%=" = "100"

let%test _ = eval "2+%" = "0.04"

let%test _ = eval "2+%=" = "2.04"

let%test _ = eval "2-%" = "0.04"

let%test _ = eval "2-%=" = "1.96"

(* stack length = 3 *)
let%test _ = eval "3+6%" = "0.18"

let%test _ = eval "3+6%=" = "3.18"

let%test _ = eval "3-6%" = "0.18"

let%test _ = eval "3-6%=" = "2.82"

let%test _ = eval "3*6%" = "0.06"

let%test _ = eval "3*6%=" = "0.18"

let%test _ = eval "3/6%" = "0.06"

let%test _ = eval "3/6%=" = "50"

(* stack length = 4 *)
let%test _ = eval "3*6+%" = "1.08"

let%test _ = eval "3*6+%=" = "19.08"

let%test _ = eval "3*6-%" = "1.08"

let%test _ = eval "3*6-%=" = "16.92"

let%test _ = eval "3/6+%" = "0.03"

let%test _ = eval "3/6+%=" = "0.53"

let%test _ = eval "3/6-%" = "0.03"

let%test _ = eval "3/6-%=" = "0.47"

let%test _ = eval "3+6+%" = "0.54"

let%test _ = eval "3+6+%=" = "9.54"

let%test _ = eval "3+6-%" = "0.54"

let%test _ = eval "3+6-%=" = "8.46"

let%test _ = eval "3-6+%" = "-0.18"

let%test _ = eval "3-6+%=" = "-3.18"

let%test _ = eval "3-6-%" = "-0.18"

let%test _ = eval "3-6-%=" = "-2.82"

(* stack length = 5 *)
let%test _ = eval "2+3+4%" = "0.2"

let%test _ = eval "2+3+4%=" = "5.2"

let%test _ = eval "2+3-4%" = "0.2"

let%test _ = eval "2+3-4%=" = "4.8"

let%test _ = eval "2-3+4%" = "-0.04"

let%test _ = eval "2-3+4%=" = "-1.04"

let%test _ = eval "2-3-4%" = "-0.04"

let%test _ = eval "2-3-4%=" = "-0.96"

let%test _ = eval "2+3*4%" = "0.04"

let%test _ = eval "2+3*4%=" = "2.12"

let%test _ = eval "2+3/4%" = "0.04"

let%test _ = eval "2+3/4%=" = "77"

let%test _ = eval "2-3*4%" = "0.04"

let%test _ = eval "2-3*4%=" = "1.88"

let%test _ = eval "2-3/4%" = "0.04"

let%test _ = eval "2-3/4%=" = "-73"
