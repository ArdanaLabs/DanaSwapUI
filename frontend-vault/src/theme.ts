import { createTheme, responsiveFontSizes } from "@mui/material/styles"
import createBreakpoints from "@mui/system/createTheme/createBreakpoints"

// custom colors

const primaryNight = {
  light: "linear-gradient(89.62deg, #72D2F2 0.3%, #6077FF 99.64%)",
  main: "#FFFFFF",
  dark: "#3142A3",
}

const primaryDay = {
  light: "linear-gradient(89.62deg, #000A4F 0.3%, #3C4DC5 99.64%)",
  main: "#041484",
  dark: "linear-gradient(90.19deg, #2F3DA0 27.19%, #73D6F1 99.87%)",
}

const secondaryNight = {
  light: "#131B59",
  main: "#FFFFFF",
  dark: "rgba(24, 33, 100, 0.5)",
}

const secondaryDay = {
  light: "linear-gradient(180deg, rgba(255, 255, 255, 0) 0%, #FFFFFF 100%)",
  main: "#636060",
  dark: "#A5A5A5",
}

const errorNight = {
  light: "#FFEAEA",
  main: "#FF2C2C",
}

const errorDay = {
  light: "#FFEAEA",
  main: "#FF2C2C",
}

const successNight = {
  light: "#CEFFF0",
  main: "#199F03",
}
const successDay = {
  light: "#CEFFF0",
  main: "#199F03",
}

const warningNight = {
  main: "#FFD8AA",
}
const warningDay = {
  main: "#FF2C2C",
}

const infoNight = {
  light: "linear-gradient(90deg, #72D2F2 0%, #6077FF 100%)",
  main: "#69A3FA",
  dark: "#72D2F3",
}

const infoDay = {
  light: "linear-gradient(90deg, #000A4F 0%, #3C4DC5 100%)",
  main: "#202E8D",
  dark: "#3651CD",
}

const backgroundNight = {
  default: "#010730",
  paper: "#2F3F9D",
}
const backgroundDay = {
  default: "#F9FBFF",
  paper: "#FFFFFF",
}

const common = {
  black: "#131B59",
  white: "#ffffff",
}
// breakpoints
const breakpoints = createBreakpoints({})

// typography
// TODO: https://mui.com/customization/typography/#responsive-font-sizes
// TODO: https://mui.com/customization/typography/#adding-amp-disabling-variants
const typography = {
  fontFamily: "Brandon Grotesque, Museo Sans, fantasy, sans-serif",
  h1: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 50,

    [breakpoints.down("sm")]: {
      fontSize: 30,
    },
  },
  h2: {},
  h3: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 700,
    fontSize: 20,

    [breakpoints.down("sm")]: {
      fontSize: 16,
    },
  },
  h4: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: "normal",
    fontSize: 20,
  },
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
// TODO: https://stackoverflow.com/questions/61120760/how-to-extend-material-ui-theme-with-typescript
export const darkTheme = responsiveFontSizes(
  createTheme({
    palette: {
      mode: "dark",
      primary: primaryNight,
      secondary: secondaryNight,
      info: infoNight,
      error: errorNight,
      success: successNight,
      warning: warningNight,
      common: common,
      background: backgroundNight,
    },
    typography,
  })
)

export const lightTheme = responsiveFontSizes(
  createTheme({
    palette: {
      mode: "light",
      primary: primaryDay,
      secondary: secondaryDay,
      info: infoDay,
      error: errorDay,
      success: successDay,
      warning: warningDay,
      common: common,
      background: backgroundDay,
    },
    typography,
  })
)

const theme = { lightTheme, darkTheme }

export default theme
