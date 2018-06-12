/**
 * Webpack config.
 *
 * @format
 * @copyright {{ year }}
 * @author {{ author }}
 * @see {@link https://webpack.js.org|webpack docs}
 * @requires css-loader
 * @requires file-loader
 * @requires html-loader
 * @requires html-webpack-plugin
 * @requires markdown-loader
 * @requires node-sass
 * @requires raw-loader
 * @requires sass-loader
 * @requires style-loader
 * @requires ts-loader
 * @requires typescript
 * @requires webpack
 */

const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');

module.exports = {
  mode: 'development',
  devtool: 'inline-source-map', // for typescript & sass
  entry: './src/index.js',
  plugins: [new HtmlWebpackPlugin()],
  resolve: {
    // add '.ts' as resolvable extensions
    extensions: ['.ts', '.js', '.json'],
  },
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: 'bundle.js',
  },
  module: {
    rules: [
      {
        test: /\.scss$/,
        use: [
          { loader: 'style-loader' },
          {
            loader: 'css-loader',
            options: {
              sourceMap: true,
            },
          },
          {
            loader: 'sass-loader',
            options: {
              sourceMap: true,
            },
          },
        ],
      },
      {
        test: /\.(png|jpg|gif)$/,
        use: [
          {
            loader: 'file-loader',
            options: {},
          },
        ],
      },
      {
        test: /\.css$/,
        use: [{ loader: 'style-loader' }, { loader: 'css-loader' }],
      },
      {
        test: /\.[jt]sx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
      {
        test: /\.html$/,
        use: {
          loader: 'html-loader',
          options: {
            attrs: [':data-src'],
          },
        },
      },
      { test: /\.txt$/, use: 'raw-loader' },
      {
        test: /\.md$/,
        use: [
          {
            loader: 'html-loader',
          },
          {
            loader: 'markdown-loader',
            options: {
              smartypants: true,
              smartyLists: true,
            },
          },
        ],
      },
    ],
  },
};

// vim:foldmethod=indent:foldlevel=1:
