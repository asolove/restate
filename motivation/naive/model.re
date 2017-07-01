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

type state = {
  pending: option (float, operation),
  decimal: bool,
  lastInput: action,
  readout: string,
  readoutEditable: bool
};

let initialState: state = {
  pending: None,
  decimal: false,
  lastInput: Cancel,
  readout: "0.",
  readoutEditable: true
};

let evaluate n1 op n2 =>
  switch op {
  | Add => n1 +. n2
  | Subtract => n1 -. n2
  | Multiply => n1 *. n2
  | Divide => n1 /. n2
  };

let update state action => {
  switch action {
  | Cancel => initialState
  | CancelEntry => {...state, readout: "0.", decimal: false, lastInput: action}
  | Decimal => {
    let readout = switch state.lastInput {
      | Op Subtract => "-0."
      | Digit _d => state.readout
      | _else => "0."
    };
    {...state, readout, decimal: true, lastInput: Digit 0}
  }
  | Digit n => {
    /* this is totally wrong if any operation was before this */
    let startReadout = if state.readoutEditable {
      state.readout
    } else {
      "0."
    };
    let readout = if state.decimal {
      startReadout ^ (string_of_int n);
    } else {
      let integralPart = if (startReadout == "0.") {
        ""
      } else {
        String.sub startReadout 0 (String.index startReadout  '.')
      };
      integralPart ^ (string_of_int n) ^ "."
    };
    { ...state, readout, readoutEditable: true, lastInput: action }
  }
  | Percent => {
    let readout = string_of_float ((float_of_string state.readout) /. 100.0);
    { ...state, readout, readoutEditable: false }
  }
  | Equal => {
    switch state.pending {
    | None => state
    | Some (n, o) => {
      ...state,
      pending: None,
      readout: string_of_float (evaluate n o (float_of_string state.readout)),
      readoutEditable: false,
      decimal: false
    }
    }
  }
  | Op op => {
    switch state.pending {
    | None => {...state, pending: Some (float_of_string state.readout, op), readout: "0.", decimal: false }
    | Some (savedNumber, savedOp) => {
      let result = evaluate savedNumber savedOp (float_of_string state.readout);
      {...state, pending: Some (result, op), readout: string_of_float result, readoutEditable: false, decimal: false}
    }
    }
  }
  }
};

let test = fun() => {
  let assertEqual a b msg => if (a == b) { () } else { failwith msg };
  let stateAfter actions => List.fold_left update initialState actions;

  let newNumberAfterPercent = [Digit 2, Percent, Digit 5];
  assertEqual (stateAfter newNumberAfterPercent).readout "5." "After percent, start entering new number.";
  
  let newNumberAfterOperation = [Digit 1, Op Add, Digit 1, Op Add, Digit 1, Digit 1];
  assertEqual (stateAfter newNumberAfterOperation).readout "11." "After previous operation resolved, number is editable";
  
  /* fixme: fails */
  let subtractForNegation = [Digit 5, Op Multiply, Op Subtract, Digit 2];
  assertEqual (stateAfter subtractForNegation).readout "-10." "Subtraction works for negation at beginning of operand";
};

/*
test ();
*/
