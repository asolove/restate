open Operations;

type doneState = 
  | Start string
  | Result string
  ;

let name = "Statechart";

type t =
  | Done doneState
  | NegOp1
  | Operand1 Operand.t
  | OperatorEntered operation float
  | NegOp2 operation float
  | Operand2 Operand.t operation float
  ;

let showState = fun
  | Done (Start s) => "Done Start " ^ s
  | Done (Result s) => "Done Result " ^ s
  | NegOp1 => "NegOp1"
  | Operand1 o => "Operand1 " ^ Operand.showState o
  | OperatorEntered _ _ => "OperationEntered"
  | NegOp2 _ _ => "NegOp1"
  | Operand2 _ _ _ => "Operand2"
  ;

let readout = fun 
  | Done (Start s) => s
  | Done (Result s) => s
  | NegOp1 => "-0."
  | Operand1 o => Operand.readout o
  | OperatorEntered _o _f => "0."
  | NegOp2 _op _f => "-0."
  | Operand2 o _op _f => Operand.readout o
  ;

let initialState = Done (Start "0.");

let percent s => string_of_float ((float_of_string s) /. 100.0);

let update state action =>
  switch (state, action) {
  | (_s, Cancel) => initialState

  | (Done _, Digit d) => Operand1 (Operand.fromDigit d Positive)
  | (Done _, Decimal) => Operand1 (Operand.fromDecimal Positive)
  | (Done (Start _), Op Subtract) => NegOp1

  | (Done (Result s), Op op) => OperatorEntered op (float_of_string s)

  | (NegOp1, CancelEntry) => Done (Start "0.")
  | (NegOp1, Digit d) => Operand1 (Operand.fromDigit d Negative)
  | (NegOp1, Decimal) => Operand1 (Operand.fromDecimal Negative)

  | (Operand1 _, CancelEntry) => Done (Start "0.")
  | (Operand1 o1, Percent) => Done (Start (string_of_float (Operand.value o1 /. 100.0)))
  | (Operand1 o1, Op op) => OperatorEntered op (Operand.value o1)
  | (Operand1 o1, action) => Operand1 (Operand.update o1 action)
  
  | (OperatorEntered op f, Op Subtract) => NegOp2 op f
  | (OperatorEntered _op f, Op op) => OperatorEntered op f
  | (OperatorEntered op f, Digit d) => Operand2 (Operand.fromDigit d Positive) op f

  | (NegOp2 op f, CancelEntry) => OperatorEntered op f
  | (NegOp2 op f, Digit d) => Operand2 (Operand.fromDigit d Negative) op f
  | (NegOp2 op f, Decimal) => Operand2 (Operand.fromDecimal Negative) op f

  | (Operand2 n2 op n1, Equal) => Done (Result (string_of_float (evaluate n1 op (Operand.value n2))))
  | (Operand2 _n op n1, CancelEntry) => OperatorEntered op n1
  | (Operand2 n2 op1 n1, Op op2) => OperatorEntered op2 (evaluate n1 op1 (Operand.value n2))
  | (Operand2 n2 op n1, action) => Operand2 (Operand.update n2 action) op n1

  | (state, action) => {
    failwith ("Cannot perform action " ^ (showAction action) ^ " in state " ^ (showState state))
  }
  };