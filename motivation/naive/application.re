module NaiveView = View.Make(Updater.NaiveUpdater);

ReactDOMRe.renderToElementWithId <NaiveView /> "index";

module NaiveTests = Tests.Make(Updater.NaiveUpdater);
NaiveTests.test();