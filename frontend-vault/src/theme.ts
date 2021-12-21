import {
  unstable_createMuiStrictModeTheme as createMuiTheme,
  responsiveFontSizes,
  ThemeOptions,
} from "@material-ui/core"
import createBreakpoints from "@material-ui/core/styles/createBreakpoints"
import { merge } from "lodash"

// custom colors

const primaryLightDay =
  "linear-gradient(89.62deg, #000A4F 0.3%, #3C4DC5 99.64%)"
const primaryLightNight =
  "linear-gradient(89.62deg, #72D2F2 0.3%, #6077FF 99.64%)"

const primaryMainDay = "#041484"
const primaryMainNight = "#FFFFFF"

const primaryDarkDay =
  "linear-gradient(90.19deg, #2F3DA0 27.19%, #73D6F1 99.87%)"
const primaryDarkNight = "linear-gradient(0deg, #3142A3, #3142A3)"
// const primaryDarkDay = "linear-gradient(180deg, #5F72FF 0%, #202F9A 100%)";
// const primaryDarkNight = "linear-gradient(180deg, #73D6F1 0%, rgba(115, 214, 241, 0) 100%)";

const secondaryMainDay = "#636060"
const secondaryMainNight = "#FFFFFF"

const secondaryLightDay =
  "linear-gradient(180deg, rgba(255, 255, 255, 0) 0%, #FFFFFF 100%)"
const secondaryLightNight = "#131B59"

const secondaryDarkDay = "#A5A5A5"
const secondaryDarkNight = "rgba(24, 33, 100, 0.5)"

const infoLightDay = "linear-gradient(90deg, #000A4F 0%, #3C4DC5 100%)"
const infoLightNight = "linear-gradient(90deg, #72D2F2 0%, #6077FF 100%)"

const infoMainDay = "#202E8D"
const infoMainNight = "#69A3FA"

const infoDarkDay = "#3651CD"
const infoDarkNight = "#72D2F3"

const backgroundDay = "#F9FBFF"
const backgroundNight = "#010730"

const backgroundPaperDay = "#FFFFFF"
// "linear-gradient(359deg, #B9D4FF -129.98%, #FFFFFF 99.14%)"

const backgroundPaperNight = "#2F3F9D"

const textPrimaryDay = "#235DF4"
const textPrimaryNight = "#FFFFFF"

const textSecondaryDay = "#636060"
const textSecondaryNight = "#FFFFFF"

const textHintDay = "#636060"
const textHintNight = "#73D6F1"

const black = "#131B59"
const white = "#ffffff"

// breakpoints
const xl = 1920
const lg = 1280
const md = 960
const sm = 700
const xs = 0

const breakpoints = createBreakpoints({})

// typography
const typography = {
  h1: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 50,

    [breakpoints.down("xs")]: {
      fontSize: 30,
    },
  },
  h2: {},
  h3: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 20,
  },
  h4: {},
  h5: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 14,
  },
  h6: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "normal",
    fontSize: 14,
  },
  subtitle1: {},
  subtitle2: {},
  body1: {},
  body2: {},
}

function createTheme(
  custom: any,
  options?: ThemeOptions | undefined,
  ...args: object[]
) {
  return createMuiTheme(merge(custom, options), ...args)
}

export const lightTheme = responsiveFontSizes(
  createTheme({
    palette: {
      type: "light",
      primary: {
        light: primaryLightDay,
        main: primaryMainDay,
        dark: primaryDarkDay,
      },
      secondary: {
        light: secondaryLightDay,
        main: secondaryMainDay,
        dark: secondaryDarkDay,
      },
      info: {
        light: infoLightDay,
        main: infoMainDay,
        dark: infoDarkDay,
      },
      common: {
        black,
        white,
      },
      text: {
        primary: textPrimaryDay,
        secondary: textSecondaryDay,
        hint: textHintDay,
      },
      background: {
        default: backgroundDay,
        paper: backgroundPaperDay,
      },
    },
    typography,
    breakpoints: {
      values: {
        xl,
        lg,
        md,
        sm,
        xs,
      },
    },
  })
)

export const darkTheme = responsiveFontSizes(
  createTheme({
    palette: {
      type: "dark",
      primary: {
        light: primaryLightNight,
        main: primaryMainNight,
        dark: primaryDarkNight,
      },
      secondary: {
        light: secondaryLightNight,
        main: secondaryMainNight,
        dark: secondaryDarkNight,
      },
      info: {
        light: infoLightNight,
        main: infoMainNight,
        dark: infoDarkNight,
      },
      common: {
        black,
        white,
      },
      text: {
        primary: textPrimaryNight,
        secondary: textSecondaryNight,
        hint: textHintNight,
      },
      background: {
        default: backgroundNight,
        paper: backgroundPaperNight,
      },
    },
    typography,
    breakpoints: {
      values: {
        xl,
        lg,
        md,
        sm,
        xs,
      },
    },
  })
)

const theme = { lightTheme, darkTheme }

export default theme
