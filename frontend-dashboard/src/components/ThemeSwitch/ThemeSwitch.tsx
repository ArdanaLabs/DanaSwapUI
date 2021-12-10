import React from "react"
import cx from "classnames"
import { Box } from "@material-ui/core"
import { makeStyles } from "@material-ui/core/styles"

import { useDarkModeManager } from "state/user/hooks"

import ICO_Dark from "assets/svg/moon.svg"
import ICO_Light from "assets/svg/sun.svg"

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
  },
}))

const ThemeSwitch: React.FC = () => {
  const [darkMode, setDarkMode] = useDarkModeManager()
  const classes = useStyles()

  const toggleMode = () => {
    setDarkMode(!darkMode)
  }

  return (
    <Box
      className={cx(classes.self)}
      flexDirection={!darkMode ? "row" : "row-reverse"}
      onClick={toggleMode}
    >
      <Box className={cx(classes.switchLabel)}>
        {!darkMode ? "DARK MODE" : "LIGHT MODE"}
      </Box>
      <Box className={cx(classes.switchIcon)}>
        <img src={!darkMode ? ICO_Dark : ICO_Light} alt="Theme switch icon" />
      </Box>
    </Box>
  )
}

export default ThemeSwitch
