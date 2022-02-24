import React from "react"
import cx from "classnames"
import { Box } from "@material-ui/core"
import { makeStyles } from "@material-ui/core/styles"

import * as O from "fp-ts/Option"
import { pipe } from "fp-ts/function"

import * as Theme from "Data/User/Theme"

import { useUserThemeManager } from "state/user/hooks"

import ICO_Dark from "assets/svg/moon.svg"
import ICO_Light from "assets/svg/sun.svg"

// If the theme list goes beyond 2 styles, this toggler will no longer work

const useStyles = makeStyles(({ palette }) => ({
  self: {
    margin: "0 10px",
    padding: 5,
    cursor: "pointer",
    borderRadius: "100px",
    display: "flex",
    alignItems: "center",
    width: "175px",
    background: palette.primary.light,
  },
  switchIcon: {
    "borderRadius": "50%",
    "background": "#FFFFFF",
    "padding": 5,
    "width": 28,
    "height": 28,
    "display": "flex",
    "justifyContent": "center",
    "alignItems": "center",

    "& img": {
      width: "14px",
      height: "14px",
    },
  },
  switchLabel: {
    fontSize: "12px",
    fontWeight: 700,
    fontFamily: "Museo Sans",
    lineHeight: "16px",
    color: "#FFFFFF",
    flexGrow: 5,
    textAlign: "center",
    textTransform: "uppercase",
  },
}))

const ThemeSwitch: React.FC = () => {
  const [userTheme, setUserTheme] = useUserThemeManager()
  const classes = useStyles()

  // only relevant while theme is binary
  const isLightTheme: boolean = Theme.Eq.equals(userTheme, Theme.Theme.Light)

  const toggleMode = () => {
    return pipe(
      userTheme,
      Theme.succ,
      O.getOrElse(() => Theme.Bounded.top),
      setUserTheme
    )
  }

  return (
    <Box
      className={cx(classes.self)}
      flexDirection={isLightTheme ? "row" : "row-reverse"}
      onClick={toggleMode}
    >
      <Box className={cx(classes.switchLabel)}>
        {isLightTheme ? "Dark" : "Light"} mode
      </Box>
      <Box className={cx(classes.switchIcon)}>
        <img src={isLightTheme ? ICO_Dark : ICO_Light} alt="" />
      </Box>
    </Box>
  )
}

export default ThemeSwitch
