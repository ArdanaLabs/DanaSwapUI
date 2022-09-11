// refer: https://nx-plugins.netlify.app/derived/esbuild.html#esbuild
const esbuild = require("esbuild")

const copyStaticFiles = require("esbuild-copy-static-files")
const { html } = require("@esbuilder/html")

const compileEJSToHTML = require("./plugins/compile-ejs-to-html")
const processCSS = require("./plugins/process-css")
const minifyHTML = require("./plugins/minify-html")

const outdir = "build"

;(async () => {
  await esbuild
    .build({
      entryPoints: ["views/pages/Home/index.ejs"],
      outdir: outdir,
      minify: true,
      sourcemap: false,
      bundle: true,
      write: false,
      loader: { ".ejs": "file" },
      plugins: [
        compileEJSToHTML({
          dest: `${outdir}`,
        }),
        minifyHTML({
          src: `${outdir}`,
          dest: `${outdir}`,
        }),
        copyStaticFiles({
          src: "assets",
          dest: `${outdir}/assets`,
        }),
        processCSS({
          src: "assets/css",
          dest: `${outdir}/assets/css`,
        }),
      ],
      assetNames: "[name]",
    })
    .catch((e) => {
      console.error(e)
      process.exit(1)
    })
  await esbuild
    .build({
      entryPoints: [`${outdir}/index.html`],
      outdir: outdir,
      minify: true,
      sourcemap: false,
      bundle: true,
      write: false,
      plugins: [
        html(),
        copyStaticFiles({
          src: `${outdir}/index.html`,
          dest: `${outdir}/404.html`,
        }),
      ],
      assetNames: "[name]",
    })
    .catch((e) => {
      console.error(e)
      process.exit(1)
    })
})()
