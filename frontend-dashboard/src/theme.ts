import {
  unstable_createMuiStrictModeTheme as createMuiTheme,
  responsiveFontSizes,
  ThemeOptions,
} from "@material-ui/core"
import createBreakpoints from "@material-ui/core/styles/createBreakpoints"
import { merge } from "lodash"

// custom colors

const primaryDark = { main: "#FFFFFF" }
const primaryLight = { main: "#1E2769" }

const secondaryDark = { main: "#73D6F1", dark: "#5F72FF" }
const secondaryLight = { main: "#314BC7", dark: "#030D53" }

const errorDark = { main: "#FF6666" }
const errorLight = { main: "#FF6666" }

const successDark = { main: "#97FF66" }
const successLight = { main: "#97FF66" }

const infoDark = { main: "#253082AA" }
const infoLight = { main: "#3A89D919" }

const backgroundDark = { default: "#080E42", paper: "#253082AA" }
const backgroundLight = { default: "#F5F5F5", paper: "#3A89D919" }

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

const breakpoints = createBreakpoints({})

const typography = {
  h1: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 30,
  },
  h3: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 20,
  },
  h5: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: "normal",
    fontSize: 16,

    [breakpoints.down("xs")]: {
      fontSize: 12,
    },
  },
  h2: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 20,
  },
  h4: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 18,

    [breakpoints.down("xs")]: {
      fontSize: 12,
    },
  },
  h6: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 13,

    [breakpoints.down("xs")]: {
      fontSize: 10,
    },
  },
  subtitle1: {},
  subtitle2: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 18,
  },
  body1: {},
  body2: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 13,
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
      error: errorDark,
      success: successDark,
      info: infoDark,
      text: textDark,
      background: backgroundDark,
      common,
    },
    typography,
  })
)

export const lightTheme = responsiveFontSizes(
  createTheme({
    palette: {
      type: "light",
      primary: primaryLight,
      secondary: secondaryLight,
      error: errorLight,
      success: successLight,
      info: infoLight,
      text: textLight,
      background: backgroundLight,
      common,
    },
    typography,
  })
)

const theme = { lightTheme, darkTheme }

export default theme
