var path = require('path');

// `npm run build` makes the minified bundles that are committed and shipped:
// the widget, and the board's code highlighter; `npm run dev` / `npm run
// watch` add a source map for local debugging.
const outputs = {
    targetsboard: 'htmlwidgets/targetsboard.js',
    highlight: 'app/www/highlight.js'
};

module.exports = (env, argv) => ({
    entry: {
        targetsboard: path.join(__dirname, 'srcjs', 'targetsboard.jsx'),
        highlight: path.join(__dirname, 'srcjs', 'highlight.js')
    },
    output: {
        path: path.join(__dirname, 'inst'),
        filename: (chunk) => outputs[chunk.chunk.name]
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
