/**
 * Webpack config file.
 *
 * Webpack will bundle all comonenets of the application and output it into dist/.
 *
 * @author Norbert Logiewa
 * @license MIT
 * @copyright nl253 2017
 *
 * The build dependencies
 * ----------------------
 * @requires babel-cli
 * @requires babel-core
 * @requires babel-loader
 * @requires babel-polyfill
 * @requires babel-preset-env
 * @requires webpack
 * @requires whatwg-fetch
 * @requires git://github.com/mishoo/UglifyJS2#harmony-v2.8.22
 *
 * Typescript
 * ----------
 * @requires typescript
 * @requires ts-loader
 * @requires @types/node
 * @requires @types/jest
 * @requires @types/body-parser
 * @requires @types/validator
 * @requires @types/mongoose
 * @requires @types/passport
 * @requires @types/express-session
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
 * Rules for all filetypes.
 */

const fileRule = {
  test: /\.(png|jpg|gif)$/,
  exclude: globals.excludes,
  use: [{
    loader: 'file-loader',
    options: {
      outputPath: ''
    }
  }]
}

// load as is ie utf-8 file content
const rawRule = {
  test: /\.txt$|LICENSE/,
  exclude: globals.excludes,
  use: 'raw-loader'
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
            node: 'current',
            browsers: ['last 4 versions']
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
 * Jsx. Looks for *.jsx files.
 * Will also optimize the build.
 * After jsx is transformed into js, jsRule starts to apply.
 * NOTE the react preset includes *.jsx to *.js
 * transformation as well as flow type stripping.
 */

let jsxRule = {
  test: /\.jsx$/,
  exclude: globals.excludes,
  use: [{
    loader: 'babel-loader',
    options: {
      presets: [
        'react',
        'react-optimize', ['env', {
          targets: {
            node: '7',
            browsers: ['last 4 versions']
          }
        }]
      ],
      minified: true,
      compact: true,
      sourceMaps: true,
      comments: false,
      ignore: ['node_modules/**', '*.ts']
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
        jsx: 'React',
        lib: ['es2015', 'es2016', 'es2017'],
        moduleResolution: 'node',
        removeComments: true,
        sourceMap: true,
        types: [
          'node',
          'passport',
          'express-session'
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

const tsxRule = {
  test: /\.tsx$/,
  exclude: globals.excludes,
  use: jsRule.use + [{
    loader: 'ts-loader',
    options: {
      compilerOptions: {
        jsx: 'React',
        importHelpers: true,
        lib: ['es2015', 'es2016', 'es2017', 'dom'],
        module: 'commonjs',
        removeComments: true,
        sourceMap: true,
        types: [
          'node',
          'passport',
          'express-session'
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
    rules: [
      jsxRule,
      tsRule,
      tsxRule,
      jsRule,
      rawRule
    ]
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
