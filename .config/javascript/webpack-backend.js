/**
 * Webpack config file.
 *
 * Webpack will bundle all comonenets of the application and output it into dist/.
 *
 * @author Norbert Logiewa
 * @license MIT
 * @copyright nl253 2017
 *
 * Build dependencies
 * ------------------
 * @requires webpack
 *
 *  Javascript
 *  ----------
 * @requires babel-cli
 * @requires babel-core
 * @requires babel-loader
 * @requires babel-polyfill
 * @requires babel-preset-env
 *
 * Typescript
 * ----------
 * @requires typescript
 * @requires ts-loader
 * @requires @types/body-parser
 * @requires @types/express-session
 * @requires @types/jest
 * @requires @types/mongoose
 * @requires @types/node
 * @requires @types/passport
 * @requires @types/validator
 *
 * Other
 * -----
 * @requires whatwg-fetch
 * @requires git://github.com/mishoo/UglifyJS2#harmony-v2.8.22
 */

/**
 * Standard library.
 */

const path = require('path')

/**
 * 3rd party.
 */

const webpack = require('webpack')

/**
 * Webpack plugins.
 */

// minify and mangle output
const UglifyJSPlugin = require('uglifyjs-webpack-plugin')

// common to all rules.
const globals = {
  excludes: /node_modules|bower_components/
}


/**
 * Javascript. Use babel to compile to lower versions of ECMAScript.
 * Set env to node and last 4 versions of browsers.
 */

const jsRule = {
  test: /\.js$/,
  exclude: globals.excludes,
  use: [{
    loader: 'babel-loader',
    options: {
      presets: [
        ['env', {
          targets: {
            node: '6.11.3',
            chrome: 49,
            safari: 9,
            firefox: 52,
            edge: 14
          }
        }]
      ],
      minified: true,
      compact: true,
      sourceMaps: true,
      comments: false
    }
  }]
}

/**
 * Typescript.
 * Adds types for node, express, jest
 * as well as: dom, es[567] support
 */

const tsRule = {
  test: /\.ts$/,
  exclude: globals.excludes,
  use: jsRule.use + [{
    loader: 'ts-loader',
    options: {
      compilerOptions: {
        importHelpers: true,
        lib: ['ES2015', 'ES2016', 'ES2017', 'ESNext'],
        moduleResolution: 'node',
        removeComments: true,
        sourceMap: true,
        types: [
          'node',
          'passport',
          'express-session',
          'express',
          'jest',
          'validator',
          'body-parser',
          'mongoose',
          'faker'
        ]
      }
    }
  }]
}


module.exports = {
  entry: {
    app: [
      'whatwg-fetch', // register polyfills:
      'babel-polyfill', // fetch (from GitHub) + a bunch of babel polyfills
      './src/server.js'
    ]
  },
  target: 'node',
  output: {
    publicPath: '/',
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js'
  },
  module: {
    rules: [tsRule, jsRule]
  },
  devtool: 'source-map',
  plugins: [
    new webpack.HotModuleReplacementPlugin({
      port: 3000
    }),
    new UglifyJSPlugin({
      uglifyOptions: {
        ie8: false,
        ecma: 8,
        mangle: true,
        compress: true,
        warnings: true
      }
    })
  ]
}
