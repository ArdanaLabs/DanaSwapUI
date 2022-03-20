import React from "react"
import { makeStyles } from "@mui/styles"
import { useTheme } from "@mui/system"
import { Box, Theme } from "@mui/material"

import { useDarkModeManager } from "state/user/hooks"
import { ReactComponent as SunIcon } from "assets/image/svgs/sun.svg"

const useStyles = makeStyles((theme: Theme) => ({
  self: {
    display: "flex",
    alignItems: "center",
    justifyContent: "space-between",
    width: "75px",
    cursor: "pointer",
    borderWidth: 3,
    borderStyle: "solid",
    borderColor: theme.palette.primary.main,
    filter: "drop-shadow(0px 4px 4px rgba(0, 0, 0, 0.25))",
    borderRadius: "100px",
    padding: "5px",
  },
  preIcon: {
    marginLeft: "5px",
    width: "20px",

    [`& path`]: {
      fill: theme.palette.primary.main,
    },
  },
  status: {
    width: "25px",
    height: "25px",
    background: theme.palette.info.light,
    borderRadius: "100px",
    boxShadow: "0px 4px 4px rgba(0, 0, 0, 0.25)",
  },
}))

const ThemeSwitch: React.FC = () => {
  const theme = useTheme()
  const [darkMode, setDarkMode] = useDarkModeManager()
  const classes = useStyles(theme)

  const toggleMode = () => {
    setDarkMode(!darkMode)
  }

  return (
    <Box className={classes.self} onClick={toggleMode}>
      <SunIcon className={classes.preIcon} />
      <Box className={classes.status} />
    </Box>
  )
}

export default ThemeSwitch
