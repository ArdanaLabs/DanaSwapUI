import React from "react"
import cx from "classnames"
import { Box } from "@material-ui/core"
import { makeStyles } from "@material-ui/core/styles"

import { useDarkModeManager } from "state/user/hooks"

import ICO_Light from "assets/imgs/sun.svg"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    margin: "0 10px",
    padding: "10px 20px",
    cursor: "pointer",
    borderRadius: "100px",
    display: "flex",
    alignItems: "center",
    background: `linear-gradient(90deg, ${palette.secondary.main} 0%, ${palette.secondary.dark} 100%)`,
  },
  switchIcon: {
    lineHeight: "0",
  },
  switchLabel: {
    fontSize: "13px",
    fontWeight: 700,
    fontFamily: "Museo Sans",
    lineHeight: "15px",
    color: palette.common.white,
    textAlign: "center",
    marginRight: "10px",
    textTransform: "uppercase",
  },
}))

const ThemeSwitch: React.FC = () => {
  const [darkMode, setDarkMode] = useDarkModeManager()
  const classes = useStyles()

  const toggleMode = () => {
    setDarkMode(!darkMode)
  }

  return (
    <Box className={cx(classes.root)} onClick={toggleMode}>
      <Box className={cx(classes.switchLabel)}>
        {!darkMode ? "DarkMode" : "LightMode"}
      </Box>
      <Box className={cx(classes.switchIcon)}>
        <img src={ICO_Light} alt="Theme switch icon" />
      </Box>
    </Box>
  )
}

export default ThemeSwitch
