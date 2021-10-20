// Overriding CreateReactApp settings, ref: https://github.com/arackaf/customize-cra
const {
  override,
  addLessLoader,
  useBabelRc,
  // fixBabelImports,
} = require('customize-cra')

const webpack = require("webpack")
const path = require('path')

// use .eslintrc.js config
// const eslintConfig = require('./.eslintrc.js')
// const useEslintConfig = configRules => config => {
//   const updatedRules = config.module.rules.map(
//     rule => {
//       // Only target rules that have defined a `useEslintrc` parameter in their options
//       if (rule.use && rule.use.some(use => use.options && use.options.useEslintrc !== void 0)) {
//         const ruleUse = rule.use[0]
//         const baseOptions = ruleUse.options
//         const baseConfig = baseOptions.baseConfig || {}
//         const newOptions = {
//           useEslintrc: false,
//           ignore: true,
//           baseConfig: { ...baseConfig, ...configRules },
//         }
//         ruleUse.options = newOptions
//         return rule

//         // Rule not using eslint. Do not modify.
//       } else {
//         return rule
//       }
//     }
//   )

//   config.module.rules = updatedRules
//   return config
// }

// add WASM loader
const wasmLoader = () => config => {
  const wasmExtensionRegExp = /\.wasm$/;

  // make file-loader ignore WASM files
  config.resolve.extensions.push('.wasm');
  config.module.rules.forEach(rule => {
    (rule.oneOf || []).forEach(oneOf => {
      if (oneOf.loader && oneOf.loader.indexOf('file-loader') >= 0) {
        oneOf.exclude.push(wasmExtensionRegExp);
      }
    })
  })

  // add a dedicated loader for WASM
  config.module.rules.push({
    test: wasmExtensionRegExp,
    include: path.resolve(__dirname, 'src'),
    use: [{ loader: require.resolve('wasm-loader'), options: {} }]
  })

  config.plugins.push(
    new webpack.ContextReplacementPlugin(
      /@emurgo\/cardano-serialization-lib-browser/
    )
  )

  return config
}

module.exports = override(
  useBabelRc(),
  addLessLoader({
    lessOptions: {
      javascriptEnabled: true,
    }
  }),
  // useEslintConfig(eslintConfig),
  wasmLoader(),
)