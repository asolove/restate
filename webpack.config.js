const path = require('path');

module.exports = {
  entry: {
    naive: './lib/js/motivation/naive/application.js'
  },
  output: {
    path: path.join(__dirname, "bundledOutputs"),
    filename: '[name].js',
  },
};
