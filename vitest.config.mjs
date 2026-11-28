// `npm test`: the widget's logic (srcjs/*.test.js), in Node. Its components
// write JSX in .js files, which webpack's Babel reads: so does esbuild here.
export default {
  esbuild: { loader: 'jsx', include: /srcjs[\/].*\.jsx?$/, exclude: [], jsx: 'automatic' },
  test: { include: ['srcjs/**/*.test.js'] },
};
