/**
 * Frontend project webpack config file.
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
 * @requires babel-preset-react
 * @requires babel-preset-react-optimize
 * @requires webpack
 *
 * @requires file-fetch
 * @requires git://github.com/mishoo/UglifyJS2#harmony-v2.8.22
 * @requires html-loader
 * @requires html-webpack-plugin
 * @requires markdown-loader
 * @requires nunjucks-loader
 *
 * CSS
 * ----
 * @requires autoprefixer
 * @requires css-loader
 * @requires cssnano
 * @requires postcss-cssnext
 * @requires postcss-import
 * @requires postcss-loader
 * @requires raw-loader
 * @requires resolve-url-loader
 * @requires sass-loader
 * @requires style-loader
 *
 * Typescript
 * ----------
 * @requires typescript
 * @requires ts-loader
 * @requires @types/jest
 * @requires @types/react
 * @requires @types/react-dom
 * @requires @types/validator
 * @requires @types/faker
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

// auto-create index.html file with bundle referenced inside.
const HtmlWebpackPlugin = require('html-webpack-plugin')

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
 * Typescript. Will also handle jsx.
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
        lib: ['es2015', 'es2016', 'es2017', 'dom'],
        removeComments: true,
        sourceMap: true,
        types: [
          'jest',
          'validator'
          'react',
          'react-dom',
          'faker'
        ]
      }
    }
  }]
}

/**
 * For *.tsx files with jsx and Typescript in them.
 */

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
          'jest',
          'validator'
          'react',
          'react-dom',
          'faker'
        ]
      }
    }
  }]
}

/**
 * CSS.
 */

const cssRule = {
  test: /\.css$/,
  exclude: globals.excludes,
  use: [{
    loader: 'style-loader',
    options: {
      sourceMap: true
    }
  }, {
    loader: 'css-loader',
    options: {
      root: '/',
      modules: true,
      minimize: true,
      sourceMap: true
    }
  }, {
    loader: 'resolve-url-loader',
    options: {
      sourceMap: true
    }
  }]
}

/**
 * Works for both *.scss and *.sass files.
 * After it is converted to CSS the config from cssRule applies.
 */

const sassRule = {
  test: /\.(s[ac]ss)$/,
  exclude: globals.excludes,
  use: cssRule.use + [{
    loader: 'sass-loader',
    options: {
      sourceMap: true
    }
  }]
}

/**
 * Works for both *.pcss and *.postcss files.
 * PCSS is minified via cssnano. Autoprefixer adds vendor prefixes.
 * After it is converted to CSS the config from cssRule applies.
 */

const pcssRule = {
  test: /\.p(ost)?css$/,
  exclude: globals.excludes,
  use: cssRule.use + [{
    loader: 'postcss-loader',
    options: {
      plugins: (loader) => [
        require('postcss-import')({
          root: loader.resourcePath
        }),
        require('postcss-cssnext')(),
        require('autoprefixer')(),
        require('cssnano')()
      ]
    }
  }]
}

/**
 * Load *.html file content.
 * Additionally: minify and minimize.
 */

const htmlRule = {
  test: /\.x?html$/,
  exclude: globals.excludes,
  use: [{
    loader: 'html-loader',
    options: {
      minimize: true,
      minifyCSS: true,
      minifyJS: true,
      removeComments: true,
      removeScriptTypeAttributes: true,
      removeStyleTypeAttributes: true,
      collapseWhitespace: false
    }
  }]
}

/**
 * Nunjucks templates.
 * Convert to HTML, then use the config from htmlRule.
 */

const njkRule = {
  test: /\.(njk|nunjucks|twig|jinja2?)$/,
  exclude: globals.excludes,
  use: htmlRule.use + [
    'nunjucks-loader'
  ]
}

/**
 * Pug (Jade) templates.
 * Convert to HTML, then use the config from htmlRule.
 */

const pugRule = {
  test: /\.(pug|jade)$/,
  exclude: globals.excludes,
  use: htmlRule.use + ['pug-loader']
}

/**
 * Markdown files.
 * Add typographic quotes.  Convert to HTML, then use the config from htmlRule.
 */

const mdRule = {
  test: /\.(md|m(ark)?down)$/,
  exclude: globals.excludes,
  use: htmlRule.use + [{
    loader: 'markdown-loader',
    options: {
      smartypants: true
    }
  }]
}

/**
 */

module.exports = {
  entry: {
    app: [
      'whatwg-fetch', // register polyfills:
      'babel-polyfill' // fetch (from GitHub) + a bunch of babel polyfills
    ]
  },
  output: {
    publicPath: '/',
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle-frontend.js'
  },
  module: {
    rules: [
      jsxRule,
      tsRule,
      tsxRule,
      jsRule,
      mdRule,
      njkRule,
      cssRule,
      pcssRule,
      pugRule,
      sassRule,
      htmlRule,
      fileRule,
      rawRule
    ]
  },
  devtool: 'source-map',
  plugins: [
    new HtmlWebpackPlugin(),
    new webpack.HotModuleReplacementPlugin({
      port: 3000 // make sure it's the same port express is serving on!
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
