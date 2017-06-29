type operand =
  | Number int
  | Decimal
  | Plus | Minus
  | Times | Divide
  | Equals | Percent
  | Cancel | CancelEntry
  ;

type state = {
  op1: option operand,
  op2: option operand,
  numOps: int,
  decimal: bool,
  lastInput: string,
  opFlag: string,
  readout: string
};

let s = ReasonReact.stringToElement;
let n num => ReasonReact.stringToElement (string_of_int num);

let component = ReasonReact.statefulComponent "Calculator";

let make _children => {
  {
    ...component,
    initialState: fun () =>
      {
        op1: None,
        op2: None,
        numOps: 0,
        decimal: false,
        lastInput: "a",
        opFlag: "",
        readout: "0."
      },
    render: fun state _self => {
      <table>
        <tr>
          <td colSpan=6>
            <div className="readout">
              (s state.readout)
            </div>
          </td>
        </tr>
        <tr>
          <td> (n 7) </td>
          <td> (n 8) </td>
          <td> (n 9) </td>
          <td> </td>
          <td> (s "C") </td>
          <td> (s "CE") </td>
        </tr>
        <tr>
          <td> (n 4) </td>
          <td> (n 5) </td>
          <td> (n 6) </td>
          <td> </td>
          <td> (s "+") </td>
          <td> (s "-") </td>
        </tr>
        <tr>
          <td> (n 1) </td>
          <td> (n 2) </td>
          <td> (n 3) </td>
          <td> </td>
          <td> (s "X") </td>
          <td> (s "/") </td>
        </tr>
        <tr>
          <td colSpan=2> (n 0) </td>
          <td> (s ".") </td>
          <td> </td>
          <td> (s "=") </td>
          <td> (s "%") </td>
        </tr>
      </table>
    }
  }
};
