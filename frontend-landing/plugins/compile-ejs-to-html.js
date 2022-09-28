const ejs = require("ejs");
const fs = require("fs");
const path = require("path");
const routes = require("../routes.json");

const getPathByDir = (dir) => {
  return routes.find((route) => dir.endsWith(route.dir))?.path
}

module.exports = (options = {}) => ({
  name: "compile-ejs-to-html",
  setup(build) {
    const dest = options.dest

    build.onResolve({ filter: /.ejs/ }, (args) => {
      ejs.renderFile(args.path, {}, {}, async function (err, str) {
        const outdir = dest + getPathByDir(args.path);
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
