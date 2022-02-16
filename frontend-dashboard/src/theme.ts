import {
  unstable_createMuiStrictModeTheme as createMuiTheme,
  responsiveFontSizes,
  ThemeOptions,
} from "@material-ui/core"
import { merge } from "lodash"

// custom colors

const primaryDark = {
  light: "linear-gradient(89.62deg, #72D2F2 0.3%, #6077FF 99.64%)",
  main: "#FFFFFF",
  dark: "linear-gradient(0deg, #3142A3, #3142A3)",
}
const primaryLight = {
  light: "linear-gradient(89.62deg, #000A4F 0.3%, #3C4DC5 99.64%)",
  main: "#235DF4",
  dark: "linear-gradient(90.19deg, #2F3DA0 27.19%, #73D6F1 99.87%)",
}

const secondaryDark = {
  light: "#131B59",
  main: "#FFFFFF",
  dark: "rgba(24, 33, 100, 0.5)",
}
const secondaryLight = {
  light: "linear-gradient(180deg, rgba(255, 255, 255, 0) 0%, #FFFFFF 100%)",
  main: "#636060",
  dark: "#A5A5A5",
}

const infoDark = {
  main: "#25308280",
  dark: "linear-gradient(180deg, #73D6F1 0%, #5F72FF 99.99%, #2F3DA0 100%)",
}
const infoLight = {
  main: "#F5F5F5",
  dark: "linear-gradient(180deg, #A5A5A5 0%, #A5A5A5 54.17%, #A5A5A5 99.99%)",
}

const backgroundDark = {
  default: "#010730",
  paper: "#131B5980",
}
const backgroundLight = {
  default: "#F5F5F5",
  paper: "#FFFFFF",
}

const textDark = {
  primary: primaryDark.main,
  secondary: secondaryDark.main,
  hint: "#73D6F1",
}
const textLight = {
  primary: primaryLight.main,
  secondary: secondaryLight.main,
  hint: "#636060",
}

const common = {
  black: "#000000",
  white: "#ffffff",
}

// breakpoints
const breakpoints = {
  values: {
    xl: 1920,
    lg: 1280,
    md: 960,
    sm: 700,
    xs: 0,
  },
}

function createTheme(
  custom: any,
  options?: ThemeOptions | undefined,
  ...args: object[]
) {
  return createMuiTheme(merge(custom, options), ...args)
}

export const darkTheme = responsiveFontSizes(
  createTheme({
    palette: {
      type: "dark",
      primary: primaryDark,
      secondary: secondaryDark,
      info: infoDark,
      text: textDark,
      background: backgroundDark,
      common,
    },
    breakpoints,
  })
)

export const lightTheme = responsiveFontSizes(
  createTheme({
    palette: {
      type: "light",
      primary: primaryLight,
      secondary: secondaryLight,
      info: infoLight,
      text: textLight,
      background: backgroundLight,
      common,
    },
    breakpoints,
  })
)

const theme = { lightTheme, darkTheme }

export default theme
