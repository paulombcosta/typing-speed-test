const path = require('path');
const ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {
  entry: ["./src/index.js"],
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'index.js'
  },

  // How do you resolve modules?
  resolve: {
    // Which directories should we pretend are the current directory too?
    modules: ['node_modules'],
    // Which file extensions do we know about automatically?
    // i.e. we don't have to specify their extension at require time.
    //
    // - the empty string means we can still specify an extension at require time
    // - js files, duh
    // - elm files
    extensions: ['.js', '.elm']
  },

  plugins: [
    new ExtractTextPlugin('app.css')
  ],

  module: {
    rules: [
      // We want to output html files from our project in the output directory
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },

      // We want to load our elm files with the `elm-webpack` loader
      // (but don't load our dependencies with it, or anything in node_modules)
      // We also want to chain in `elm-hot`
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-hot-loader!elm-webpack-loader'
      },

      {
        use: ExtractTextPlugin.extract({
          fallback: "style-loader",
          use: ['css-loader', 'postcss-loader']
        }),
        test: /\.css$/
        //use: ["style-loader", "css-loader", "postcss-loader"]
      }

    ],

    // Don't try to parse elm files, because they will never `require` another module
    noParse: /\.elm$/
  },

  // Hey let's have a dev server
  devServer: {
    // We want the dev server inlined into the bundle for us
    inline: true,
    // Set the log level to only show us errors.
    // Other options are: none, minimal, normal, verbose
    stats: 'errors-only'
  }
};
