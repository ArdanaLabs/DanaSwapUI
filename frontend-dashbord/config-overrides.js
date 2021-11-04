// Overriding CreateReactApp settings, ref: https://github.com/arackaf/customize-cra
const {
  override,
  addLessLoader,
  // useBabelRc,
  // fixBabelImports,
} = require("customize-cra");

const webpack = require("webpack");
const path = require("path");

// add WASM loader
const wasmLoader = () => (config) => {
  const wasmExtensionRegExp = /\.wasm$/;

  // make file-loader ignore WASM files
  config.resolve.extensions.push(".wasm");
  config.module.rules.forEach((rule) => {
    (rule.oneOf || []).forEach((oneOf) => {
      if (oneOf.loader && oneOf.loader.indexOf("file-loader") >= 0) {
        oneOf.exclude.push(wasmExtensionRegExp);
      }
    });
  });

  // add a dedicated loader for WASM
  config.module.rules.push({
    test: wasmExtensionRegExp,
    type: 'javascript/auto',
    loaders: ['wasm-loader'],
    include: path.resolve(__dirname, "src"),
    use: [{ loader: require.resolve("wasm-loader"), options: {} }],
  });

  config.plugins.push(
    new webpack.ContextReplacementPlugin(
      /@emurgo\/cardano-serialization-lib-browser/
    )
  );
  
  return config;
};

module.exports = {
  ...override(
    // useBabelRc(),
    addLessLoader({
      lessOptions: {
        javascriptEnabled: true,
      },
    }),
    // useEslintConfig(eslintConfig),
    wasmLoader()
  ),
  experiments: {
    asyncWebAssembly: true,
    importAsync: true,
  },
};
