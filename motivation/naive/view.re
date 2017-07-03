open Operations;

let show = fun
  | Digit n => (string_of_int n)
  | Decimal => "."
  | Percent => "%"
  | Op Add => "+"
  | Op Subtract => "-"
  | Op Multiply => "*"
  | Op Divide => "/"
  | Equal => "="
  | Cancel => "C"
  | CancelEntry => "CE"
  ;

module Make = fun (Updater: Updater.Updater) => {

  /* let s = Updater.name;*/

  let component = ReasonReact.statefulComponent (Updater.name^"Calculator");

  let make _children => {
    {
      ...component,
      initialState: fun () => Updater.initialState,
      render: fun (state: Updater.t) self => {
        let perform action _payload state _self => ReasonReact.Update (Updater.update state action);
        let button action =>
          <button onClick=(self.update (perform action))>
            (ReasonReact.stringToElement (show action))
          </button>;

        <table>
          <tbody>
            <tr>
              <td colSpan=6>
                <div className="readout">
                  (ReasonReact.stringToElement (Updater.readout state))
                </div>
              </td>
            </tr>
            <tr>
              <td> (button (Digit 7)) </td>
              <td> (button (Digit 8)) </td>
              <td> (button (Digit 9)) </td>
              <td> </td>
              <td> (button Cancel) </td>
              <td> (button CancelEntry) </td>
            </tr>
            <tr>
              <td> (button (Digit 4)) </td>
              <td> (button (Digit 5)) </td>
              <td> (button (Digit 6)) </td>
              <td> </td>
              <td> (button (Op Add)) </td>
              <td> (button (Op Subtract)) </td>
            </tr>
            <tr>
              <td> (button (Digit 1)) </td>
              <td> (button (Digit 2)) </td>
              <td> (button (Digit 3)) </td>
              <td> </td>
              <td> (button (Op Multiply)) </td>
              <td> (button (Op Divide)) </td>
            </tr>
            <tr>
              <td colSpan=2> (button (Digit 0)) </td>
              <td> (button Decimal) </td>
              <td> </td>
              <td> (button Equal) </td>
              <td> (button Percent) </td>
            </tr>
          </tbody>
        </table>
      }
    }
  };

};
