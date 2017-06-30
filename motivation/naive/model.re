type operation =
  | Add | Subtract
  | Multiply | Divide
  | Equal
  ;

type action =
  | Digit int
  | Decimal
  | Op operation
  | Percent
  | Cancel
  | CancelEntry
  ;

type state = {
  pending: option (float, operation),
  decimal: bool,
  lastInput: action,
  readout: string
};

let initialState: state = {
  pending: None,
  decimal: false,
  lastInput: Cancel,
  readout: "0."
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
    let readout = if state.decimal {
      state.readout ^ (string_of_int n);
    } else {
      let integralPart = if (state.readout == "0.") {
        ""
      } else {
        String.sub state.readout 0 (String.index state.readout  '.')
      };
      integralPart ^ (string_of_int n) ^ "."
    };
    { ...state, readout, lastInput: action }
  }
  | Percent => {
    let readout = string_of_float ((float_of_string state.readout) /. 100.0);
    { ...state, readout }
  }
  | Op o => {
    {...state, readout: "0.", decimal: false};
    /* TODO: actually perform op in pending */
  }
  }
}