import {
  unstable_createMuiStrictModeTheme as createMuiTheme,
  ThemeOptions,
} from "@material-ui/core"
import createBreakpoints from "@material-ui/core/styles/createBreakpoints"
import { merge } from "lodash"

const breakpoints = createBreakpoints({})

// custom colors
const darkPrimary = {
  main: "#F5FCFE",
}

const darkSecondary = {
  main: "#73D6F1",
}

const darkInfo = {
  main: "#2B3992",
}

const darkBackground = {
  default: "#080E42",
  paper: "#1D277A",
}

const darkText = {
  primary: "#F5FCFE",
  secondary: "#73D6F1",
  hint: "#73D6F1",
}

const darkTypography = {
  h1: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: [70],
    color: darkPrimary.main,
    whiteSpace: "pre-line",

    [breakpoints.down("xs")]: {
      fontSize: [35],
    },
  },
  h2: {},
  h3: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: [60],
    color: darkPrimary.main,

    [breakpoints.down("xs")]: {
      fontSize: [35],
    },
  },
  h4: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: [16],
    color: darkPrimary.main,
    whiteSpace: "pre-line",
  },
  h5: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: [26],
    color: darkPrimary.main,

    [breakpoints.down("xs")]: {
      fontSize: [25],
    },
  },
  h6: {},
}

const common = {
  black: "#080E42",
  white: "#ffffff",
}

function createTheme(
  custom: any,
  options?: ThemeOptions | undefined,
  ...args: object[]
) {
  return createMuiTheme(merge(custom, options), ...args)
}

export const lightTheme = createTheme({})

export const darkTheme = createTheme({
  palette: {
    type: "dark",
    primary: darkPrimary,
    secondary: darkSecondary,
    info: darkInfo,
    text: darkText,
    background: darkBackground,
    common,
  },
  typography: darkTypography,
})

const theme = { lightTheme, darkTheme }

export default theme
