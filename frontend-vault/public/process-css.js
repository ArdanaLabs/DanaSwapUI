const fs = require("fs");
const path = require("path");
const postcss = require("postcss");
const autoprefixer = require("autoprefixer");
const csso = require("postcss-csso");
const flexbugsFixes = require("postcss-flexbugs-fixes");

module.exports = (options = {}) => ({
  name: "process-css",
  setup(build) {
    const src = options.src || "assets/css";
    const dest = options.dest || "build/assets/css";

    const filter = () => true;

    build.onEnd(() => {
      fs.cpSync(src, dest, {
        dereference: options.dereference || true,
        errorOnExist: options.errorOnExist || false,
        filter: options.filter || filter,
        force: options.force || true,
        preserveTimestamps: options.preserveTimestamps || true,
        recursive: options.recursive || true,
      });

      const files = fs.readdirSync(src);

      const minifyPromises = files.map((file) =>
        postcss([
          autoprefixer({
            flexbox: "no-2009",
            grid: true,
          }),
          flexbugsFixes(),
          csso({
            forceMediaMerge: true,
            comments: "exclaimation",
          }),
        ]).process(fs.readFileSync(path.join(src, file), "utf8"), {
          from: path.join(src, file),
        })
      );

      Promise.all(minifyPromises).then((responses) => {
        responses.forEach((response, index) => {
          fs.writeFileSync(path.join(dest, files[index]), response.css);
        });
      });
    });
  },
});
