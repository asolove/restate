
open Model;

let show = fun
  | Digit n => (string_of_int n)
  | Decimal => "."
  | Percent => "%"
  | Op Add => "+"
  | Op Subtract => "-"
  | Op Multiply => "*"
  | Op Divide => "/"
  | Op Equal => "="
  | Cancel => "C"
  | CancelEntry => "CE"
  ;


let component = ReasonReact.statefulComponent "Calculator";

let make _children => {
  {
    ...component,
    initialState: fun () => Model.initialState,
    render: fun (state: Model.state) self => {
      let perform action _payload state _self => ReasonReact.Update (Model.update state action);
      let button action =>
        <button onClick=(self.update (perform action))>
          (ReasonReact.stringToElement (show action))
        </button>;

      <table>
        <tbody>
          <tr>
            <td colSpan=6>
              <div className="readout">
                (ReasonReact.stringToElement state.readout)
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
            <td> (button (Op Equal)) </td>
            <td> (button Percent) </td>
          </tr>
        </tbody>
      </table>
    }
  }
};
