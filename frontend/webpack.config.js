const webpack = require('webpack');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const path = require('path');

/*
 * SplitChunksPlugin is enabled by default and replaced
 * deprecated CommonsChunkPlugin. It automatically identifies modules which
 * should be splitted of chunk by heuristics using module duplication count and
 * module category (i. e. node_modules). And splits the chunks…
 *
 * It is safe to remove "splitChunks" from the generated configuration
 * and was added as an educational example.
 *
 * https://webpack.js.org/plugins/split-chunks-plugin/
 *
 */

/*
 * We've enabled UglifyJSPlugin for you! This minifies your app
 * in order to load faster and run less javascript.
 *
 * https://github.com/webpack-contrib/uglifyjs-webpack-plugin
 *
 */

const UglifyJSPlugin = require('uglifyjs-webpack-plugin');

module.exports = {
	module: {
		rules: [
			{
				test: /\.css$/,

				use: [
					{
						loader: 'style-loader',

						options: {
							sourceMap: true
						}
					},
					{
						loader: 'css-loader'
					}
				]
			},
            {
                test: /\.scss/,
                use: [
                    {
                        // output to link tag
                        loader: 'style-loader'
                    },
                    // bundle css
                    {
                        loader: 'css-loader',
                        options: {
                            // prohibit `url()` in css`
                            url: false,
                            // use source map
                            sourceMap: true,
                            // 0 => no loaders
                            // 1 => postcss-loader
                            // 2 => postcss-loader, sass-loader
                            importLoaders: 2
                        }
                    },
                    {
                        loader: 'sass-loader',
                        options: {
                            sourceMap: true
                        }
                    }
                ]
            },
            {
                test: /\.elm$/,
                exclude: [
                    /elm_stuff/,
                    /node_modules/
                ],
                use: [
                    {
                        loader: 'elm-webpack-loader?verbose=true'
                    }
                ]
            },
            {
              test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
              loader: 'url-loader?limit=10000&mimetype=application/font-woff',
            },
            {
              test: /\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
              loader: 'file-loader',
            }
		],
        noParse: /\.elm$/
	},

	entry: {
		app: './src/index.js'
	},

	output: {
		filename: '[name].[chunkHash]js',
        path: path.resolve(__dirname, '../www/dist')
	},

	mode: 'development',
	plugins: [
        new UglifyJSPlugin(),
        new HtmlWebpackPlugin({
            template: './src/index.html',
            inject: 'body',
            filename: 'index.html'
        })
    ],

	optimization: {
		splitChunks: {
			cacheGroups: {
				vendors: {
					priority: -10,
					test: /[\\/]node_modules[\\/]/
				}
			},

			chunks: 'async',
			minChunks: 1,
			minSize: 30000,
			name: true
		}
	},
    devServer: {
        inline: true,
        stats: { colors: true },
    }
};
