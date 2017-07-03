open Operations;

module type Updater = {
  type t;
  let name: string;
  let initialState: t;
  let update: t => action => t;
  let readout: t => string;
};

module NaiveUpdater: Updater = {
  let name = "Naive";

  type t = {
    pending: option (float, operation),
    decimal: bool,
    lastInput: action,
    readout: string,
    readoutEditable: bool
  };

  let initialState: t = {
    pending: None,
    decimal: false,
    lastInput: Cancel,
    readout: "0.",
    readoutEditable: true
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

  let readout state => state.readout;

};
