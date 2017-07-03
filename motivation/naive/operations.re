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


let evaluate n1 op n2 =>
  switch op {
  | Add => n1 +. n2
  | Subtract => n1 -. n2
  | Multiply => n1 *. n2
  | Divide => n1 /. n2
  };
