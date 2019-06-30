type op = Op_Div | Op_Mul | Op_Sub | Op_Add

type button =
  | Button_Cancel
  | Button_Negate
  | Button_Div
  | Button_Mul
  | Button_Sub
  | Button_Add
  | Button_Eq
  | Button_Percent
  | Button_Dot
  | Button_Num of int
