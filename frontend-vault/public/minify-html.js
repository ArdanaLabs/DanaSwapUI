const minifyHtml = require("html-minifier").minify;
const fs = require("fs");
const path = require("path");

module.exports = (options = {}) => ({
  name: "minify-html",
  setup(build) {
    const src = options.src || "assets";
    const dest = options.dest || "build/assets";

    const filter = () => true;

    build.onEnd(() => {
      const files = fs
        .readdirSync(src)
        .filter((file) => file.endsWith(".html"));

      const minifyPromises = files.map((file) =>
        minifyHtml(fs.readFileSync(path.join(src, file), "utf8"), {
          removeAttributeQuotes: true,
          collapseWhitespace: true,
          removeComments: true,
          removeOptionalTags: true,
          removeRedundantAttributes: true,
          removeScriptTypeAttributes: true,
          removeTagWhitespace: true,
          useShortDoctype: true,
          minifyCSS: true,
          minifyJS: true,
        })
      );

      minifyPromises.forEach((response, index) => {
        fs.writeFileSync(path.join(dest, files[index]), response);
      });
    });
  },
});
