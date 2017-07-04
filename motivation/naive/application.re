module NaiveView = View.Make(Updater.NaiveUpdater);
module StatechartView = View.Make(Calculator_state);

ReactDOMRe.renderToElementWithId <StatechartView /> "index";

module NaiveTests = Tests.Make(Updater.NaiveUpdater);
NaiveTests.test();

module StatechartTests = Tests.Make(Calculator_state);
StatechartTests.test();