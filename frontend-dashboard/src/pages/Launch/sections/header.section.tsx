import React from "react"
import { Box, Container, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useHistory } from "react-router-dom"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"

import IMG_logo from "assets/logos/Ardana_hor_white.png"

const useStyles = makeStyles(({ palette }) => ({
  header: {
    "cursor": "pointer",
    "position": "fixed",
    "top": "0",
    "width": "100%",
    "zIndex": 1000,

    "& > div": {
      display: "flex",
      justifyContent: "space-between",
      alignItems: "center",
    },
  },

  navGroup: {
    display: "flex",
    justifyContent: "center",
  },
  navItem: {
    "margin": "10px",
    "padding": "10px 30px",
    "borderRadius": "20px",
    "background": "transparent",
    "color": "white",
    "textAlign": "center",
    "textTransform": "uppercase",
    "fontFamily": "Museo Sans",
    "fontStyle": "normal",
    "fontWeight": 700,
    "fontSize": "11px",
    "lineHeight": "100%",
    "border": "1px solid white",
    "cursor": "pointer",

    "&:hover": {
      background: "white",
      color: "#000633",
    },
  },

  active: {
    "border": "unset",
    "background": "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
    "&:hover": {
      background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",
      color: "#000633",
    },
  },
}))

export interface LaunchHeaderProps {
  nav: number
  updateNav: any
}

const LaunchHeader: React.FC<LaunchHeaderProps> = ({ nav, updateNav }) => {
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const userTheme: Theme.Theme = useUserTheme()
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const history = useHistory()

  // TODO: actually define nav

  return (
    <Box className={cx(classes.header)}>
      <Container>
        <Box onClick={() => history.push("/")}>
          <img src={IMG_logo} alt="Ardana" />
        </Box>
        <Box className={cx(classes.navGroup)}>
          <Box
            className={cx(classes.navItem, {
              [classes.active]: nav === 0,
            })}
            onClick={() => updateNav(0)}
          >
            Launch Ardana Stablecoins
          </Box>
          <Box
            className={cx(classes.navItem, {
              [classes.active]: nav === 1,
            })}
            onClick={() => updateNav(1)}
          >
            Launch DANASwap
          </Box>
          <Box
            className={cx(classes.navItem, {
              [classes.active]: nav === 2,
            })}
            onClick={() => updateNav(2)}
          >
            My Dashboard
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default LaunchHeader
