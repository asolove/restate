type operation =
  | Add | Subtract
  | Multiply | Divide
  ;

type action =
  | Digit int
  | Decimal
  | Op operation
  | Equal
  | Percent
  | Cancel
  | CancelEntry
  ;

let showAction = fun
| Digit d => "Digit " ^ (string_of_int d)
| Decimal => "Decimal"
| Op Add => "Op Add"
| Op Subtract => "Op Subtract"
| Op Multiply => "Op Multiply"
| Op Divide => "Op Divide"
| Equal => "Equal"
| Percent => "Percent"
| Cancel => "Cancel"
| CancelEntry => "CancelEntry"
;

let evaluate n1 op n2 =>
  switch op {
  | Add => n1 +. n2
  | Subtract => n1 -. n2
  | Multiply => n1 *. n2
  | Divide => n1 /. n2
  };

type sign = Positive | Negative;