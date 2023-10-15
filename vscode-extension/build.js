const esbuild = require('esbuild')

esbuild.build({
  entryPoints: ['./src/extension.ts'],
  outfile: './dist/extension.js',
  platform: "node",
  format: "cjs",
  external: ["vscode"],
  bundle: true,
  minify: true,
  sourcemap: false,
  watch: false,
})
.catch(() => process.exit(1));
