var webpack = require('webpack');

module.exports = {
  entry: __dirname + '/src/scripts/Main.elm',
  
  output: {
    path: __dirname + '/src',
    filename: 'main.js',
    library: 'Elm'
  },
  
  resolve: {
    modules: ['node_modules'],
    moduleExtensions: ['.elm']
  },
  
  module: {
    rules: [
      // {
      //   test: /\.html$/,
      //   exclude: /node_modules/,
      //   loader: 'file-loader?name=[name].[ext]'
      // },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {}
        }
      }
    ],
    
    noParse: /\.elm$/
  },
  
  plugins: [
    // new webpack.optimize.UglifyJsPlugin({
    //   uglifyOptions: {
    //     ecma: 5,
    //     parse: {
    //       bare_returns: false,
    //       ecma: 5,
    //       html5_comments: true,
    //       shebang: true
    //     },
    //     mangle: {
    //       keep_fnames: true
    //     },
    //     output: {
    //       comments: false,
    //       beautify: false
    //     },
    //     ie8: false,
    //     comments: false
    //   },
    //   parallel: {
    //     cache: true,
    //     workers: 2
    //   },
    //   sourceMap: true
    // })
  ],
  
  devServer: {
    inline: true,
    stats: 'errors-only'
  }
};