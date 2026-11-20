var path = require('path');

// `npm run build` makes the minified bundle that is committed and shipped;
// `npm run dev` / `npm run watch` add a source map for local debugging.
module.exports = (env, argv) => ({
    entry: path.join(__dirname, 'srcjs', 'targetsboard.jsx'),
    output: {
        path: path.join(__dirname, 'inst/htmlwidgets'),
        filename: 'targetsboard.js'
    },
    module: {
        rules: [
            {
                test: /\.jsx?$/,
                loader: 'babel-loader',
                options: {
                    presets: ['@babel/preset-env', '@babel/preset-react']
                }
            },
            // For CSS so that import "path/style.css"; works
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            }
        ]
    },
    externals: {
        'react': 'window.React',
        'react-dom': 'window.ReactDOM',
        'reactR': 'window.reactR'
    },
    // The bundle is served from localhost by the R package, so the size
    // limits meant for public websites don't apply.
    performance: { hints: false },
    stats: {
        colors: true
    },
    devtool: argv.mode === 'development' ? 'source-map' : false
});
