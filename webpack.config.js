module.exports = {
  entry: __dirname + '/src/scripts/Main.elm',
  
  output: {
    path: __dirname + '/src',
    filename: 'main.js',
    library: 'Elm'
  },
  
  resolve: {
    modules: ['node_modules'],
    moduleExtensions: ['.js', '.elm']
  },
  
  module: {
    rules: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
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
  
  devServer: {
    inline: true,
    stats: 'errors-only'
  }
};