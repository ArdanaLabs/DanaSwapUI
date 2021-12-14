import {
  unstable_createMuiStrictModeTheme as createMuiTheme,
  responsiveFontSizes,
  ThemeOptions,
} from "@material-ui/core"
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

const infoLightDay = "linear-gradient(89.62deg, #000A4F 0.3%, #3C4DC5 99.64%)"
const infoLightNight = "linear-gradient(89.62deg, #72D2F2 0.3%, #6077FF 99.64%)"

const infoMainDay = "#000000"
const infoMainNight = "#000000"

const infoDarkDay = "#6077FF"
const infoDarkNight = "#000A4F"

const backgroundDay = "#F9FBFF"
const backgroundNight = "#010730"

const backgroundPaperDay =
  "linear-gradient(180deg, rgba(255, 255, 255, 0) 0%, #FFFFFF 100%)"
const backgroundPaperNight = "linear-gradient(180deg, #131B59 0%, #2F3DA0 100%)"

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
    typography: {
      h1: {},
      h2: {},
      h3: {},
      h4: {},
      h5: {},
      h6: {
        fontFamily: "Brandon Grotesque",
        fontSize: 14,
        fontWeight: 700,
        color: primaryMainDay,
      },
      subtitle1: {},
      subtitle2: {},
      body1: {},
      body2: {},
    },
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
    typography: {
      h1: {},
      h2: {},
      h3: {},
      h4: {},
      h5: {},
      h6: {
        fontFamily: "Brandon Grotesque",
        fontSize: 14,
        fontWeight: 700,
        color: primaryMainNight,
      },
      subtitle1: {},
      subtitle2: {},
      body1: {},
      body2: {},
    },
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
