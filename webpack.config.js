const path = require('path');

module.exports = {
  entry: [
    './process-shim.js',
    './lib/js/bin/browser.js',
  ],
  output: {
    filename: '[name].js',
    path: path.join(__dirname, './public'),
    publicPath: '/public'
  },
  resolve: {
    alias: {
      bin: path.resolve(__dirname, 'bin/'),
    },
  },
};
