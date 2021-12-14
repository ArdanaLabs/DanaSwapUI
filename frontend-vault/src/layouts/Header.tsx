import React, { useEffect, useState } from "react"
import { Box, useMediaQuery, Container } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { useHistory } from "react-router-dom"
import { ThemeSwitch, ConnectWallet } from "components"
import DUSD_LOGO_BLUE from "assets/image/DUSD-LOGO-BLUE.png"
import DUSD_LOGO_WHITE from "assets/image/DUSD-LOGO-WHITE.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "fixed",
    top: 0,
    zIndex: 100,
    width: "100%",
    filter: "drop-shadow(0px 15px 15px rgba(0, 0, 0, 0.05))",
    mixBlendMode: "normal",
  },

  container: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    transition: "background .2s ease-in",
    padding: "20px 0px",
  },

  logo: {
    "paddingLeft": "10px",
    "display": "flex",
    "alignItems": "center",
    "cursor": "pointer",
    "& img": {
      width: "60px",

      [breakpoints.down("sm")]: {
        width: "40px",
      },
    },
  },

  toolbar: {
    display: "flex",
    alignItems: "center",
    justifyContent: "space-between",
    width: "265px",

    [breakpoints.down("xs")]: {
      width: "auto",
    },
  },
}))

const Header: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })
  const history = useHistory()
  const [bgColor, setBGColor] = useState("transparent")

  const handleScroll = () => {
    setBGColor(
      window.scrollY > 0 ? theme.palette.background.default : "transparent"
    )
  }

  useEffect(() => {
    handleScroll()
    window.addEventListener("scroll", handleScroll, { passive: true })
    return () => window.removeEventListener("scroll", handleScroll)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [theme])

  return (
    <Box className={cx(classes.root)} style={{ background: bgColor }}>
      <Container>
        <Box className={cx(classes.container)}>
          <Box className={cx(classes.logo)} onClick={() => history.push("/")}>
            <img
              src={
                theme.palette.type === "dark" ? DUSD_LOGO_WHITE : DUSD_LOGO_BLUE
              }
              alt="DANA Logo"
            />
          </Box>

          <Box className={cx(classes.toolbar)}>
            {!mobile && <ThemeSwitch />}
            <ConnectWallet />
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Header
