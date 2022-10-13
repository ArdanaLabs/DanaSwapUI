const ejs = require("ejs");
const fs = require("fs");
const path = require("path");

module.exports = (options = {}) => ({
  name: "compile-ejs-to-html",
  setup(build) {
    const outdir = options.dest;

    build.onResolve({ filter: /.ejs/ }, (args) => {
      ejs.renderFile(args.path, {}, {}, async function (err, str) {
        if (err) console.error(err);
        try {
          fs.mkdirSync(outdir, { recursive: true });
          fs.writeFileSync(`${outdir}/index.html`, str);
        } catch (e) {
          console.error(e);
        }
      });
      return { path: path.join(args.resolveDir, args.path) };
    });
  },
});
