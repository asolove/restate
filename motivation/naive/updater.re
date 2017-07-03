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
    readout: string,
    readoutEditable: bool
  };

  let initialState: t = {
    pending: None,
    decimal: false,
    readout: "0.",
    readoutEditable: true
  };

  let update state action => {
    switch action {
    | Cancel => initialState
    | CancelEntry => {...state, readout: "0.", decimal: false}
    | Decimal => {
      if (state.readoutEditable) {
        {...state, decimal: true}
      } else {
        {...state, decimal: true, readout: "0."}
      }
    }
    | Digit n => {
      let startReadout = if state.readoutEditable {
        state.readout
      } else {
        "0."
      };
      let readout = if state.decimal {
        startReadout ^ (string_of_int n);
      } else {
        let sign = if (String.get startReadout 0 == '-') { "-" } else { "" };
        let integralPart = if (startReadout == "0." || startReadout == "-0.") {
          ""
        } else {
          String.sub startReadout 0 (String.index startReadout  '.')
        };
        sign ^ integralPart ^ (string_of_int n) ^ "."
      };
      { ...state, readout, readoutEditable: true }
    }
    | Percent => {
      let readout = string_of_float ((float_of_string state.readout) /. 100.0);
      { ...state, readout, readoutEditable: false }
    }
    | Equal => {
      switch state.pending {
      | None => {...state, readoutEditable: false, decimal: false}
      | Some (n, o) => {
        pending: None,
        readout: string_of_float (evaluate n o (float_of_string state.readout)),
        readoutEditable: false,
        decimal: false
      }
      }
    }
    | Op op => {
      if (op == Subtract && state.readoutEditable && (state.readout == "0." || state.readout == "-0.")) {
        {...state, readout: if (state.readout == "-0.") { "0."} else { "-0."}}
      } else {
        switch state.pending {
        | None => {...state, pending: Some (float_of_string state.readout, op), readout: "0.", decimal: false }
        | Some (savedNumber, savedOp) => {
          let result = evaluate savedNumber savedOp (float_of_string state.readout);
          {pending: Some (result, op), readout: string_of_float result, readoutEditable: false, decimal: false}
        }
        }
      }
    }
    }
  };

  let readout state => state.readout;

};
