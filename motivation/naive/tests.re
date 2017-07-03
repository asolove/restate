open Operations;

module Make = fun(Updater: Updater.Updater) => {
  let test = fun() => {
    let assertEqual a b msg => if (a == b) { () } else { failwith (Updater.name ^ ": " ^ msg) };
    let after actions => Updater.readout (List.fold_left Updater.update Updater.initialState actions);
    let assertResult name actions answer => assertEqual (after actions) answer name;

    assertResult
      "Digit starts an operand"
      [Digit 1]
      "1.";

    assertResult
      "Subsequent digits add to end of operand"
      [Digit 1, Digit 2, Digit 3]
      "123.";

    assertResult
      "Pressing decimal makes subsequent digits move into decimal positions"
      [Digit 1, Decimal, Digit 2, Digit 3]
      "1.23";

    assertResult
      "After operand, percent divides number by 100"
      [Digit 1, Digit 2, Digit 3, Percent]
      "1.23";

    assertResult
      "After percent, next digit starts a new number"
      [Digit 2, Percent, Digit 5]
      "5.";

    assertResult
      "After previous operation resolved, number is editable"
      [Digit 1, Op Add, Digit 1, Op Add, Digit 1, Digit 1]
      "11.";

    assertResult
      "After an operation, subtraction serves to negate the upcoming operand"
      [Digit 5, Op Multiply, Op Subtract, Digit 2]
      "-2.";
  };
}