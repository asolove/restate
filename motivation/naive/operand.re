open Operations;

type t =
  | Zero sign
  | BeforeDecimal sign string
  | AfterDecimal sign string int
  ;

let signReadout = fun
  | Positive => ""
  | Negative => "-"
  ;

let readout = fun 
  | Zero sign => signReadout sign ^ "0."
  | BeforeDecimal sign n => signReadout sign ^ n ^ "."
  | AfterDecimal sign n2 n => (signReadout sign) ^ (string_of_int n) ^ "." ^ n2
  ;

let value operandState => float_of_string (readout operandState);
let showState state =>
  switch(state) {
  | Zero _ => "Zero " ^ readout state
  | BeforeDecimal _ _ => "BeforeDecimal " ^ readout state
  | AfterDecimal _ _ _ => "AfterDecimal " ^ readout state
  };
  
let fromDigit d sign => 
  if (d == 0) {
    Zero sign
  } else {
    BeforeDecimal sign (string_of_int d)
  };

let fromDecimal sign => AfterDecimal sign "" 0;


let update state action =>
  switch (state, action) {
  | (Zero s, Digit d) => BeforeDecimal s (string_of_int d)
  | (Zero s, Decimal) => AfterDecimal s "" 0

  | (BeforeDecimal s n, Digit d) => BeforeDecimal s (n ^ (string_of_int d))
  | (BeforeDecimal s n, Decimal) => AfterDecimal s "" (int_of_string n)

  | (AfterDecimal s n2 n, Digit d) => AfterDecimal s (n2^string_of_int d) n

  | (state, action) => {
    failwith ("Cannot perform action " ^ (showAction action) ^ " in state " ^ (showState state))
  }
};
