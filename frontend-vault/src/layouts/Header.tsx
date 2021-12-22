import React, { useEffect, useState } from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Typography,
  Drawer,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import Hamburger from "hamburger-react"

import { useIsDarkMode } from "state/user/hooks"
import { useHistory } from "react-router-dom"
import { ThemeSwitch, ConnectWallet, AddressCard } from "components"
import DUSD_LOGO_BLUE from "assets/image/DUSD-LOGO-BLUE.png"
import DUSD_LOGO_WHITE from "assets/image/DUSD-LOGO-WHITE.png"
import { useWallet } from "state/wallet/hooks"

const MenuList = [
  {
    text: "Your vaults",
    link: "/owner",
  },
  {
    text: "Open a new vault",
    link: "/vaults/list",
  },
]

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

      [breakpoints.down("xs")]: {
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

  menubar: {
    "& .menu > div": {
      cursor: "pointer",
      margin: "5px 10px",
      textTransform: "uppercase",
      color: palette.primary.main,
    },
  },

  drawerContainer: {
    "& .MuiDrawer-paper": {
      background: palette.background.default,
    },
  },

  drawer: {
    "textAlign": "center",
    "marginTop": "50px",

    "& h3": {
      padding: "10px 20px",
      color: palette.primary.main,
    },
  },
}))

const Header: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("xs"))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })
  const history = useHistory()
  const { address } = useWallet()
  const [bgColor, setBGColor] = useState("transparent")
  const [openMenu, setOpenMenu] = useState(false)

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
          <Box display={"flex"} alignItems={"center"}>
            <Box
              className={cx(classes.logo)}
              onClick={() => history.push("/")}
              mr={!mobile ? "30px" : "0px"}
            >
              <img
                src={
                  theme.palette.type === "dark"
                    ? DUSD_LOGO_WHITE
                    : DUSD_LOGO_BLUE
                }
                alt="DANA Logo"
              />
            </Box>
            {address && !mobile && <ThemeSwitch />}
          </Box>

          {!address && (
            <Box className={cx(classes.toolbar)}>
              {!mobile && <ThemeSwitch />}
              <ConnectWallet />
            </Box>
          )}
          {address && (
            <Box
              className={cx(classes.menubar)}
              display={"flex"}
              alignItems={"center"}
            >
              {!mobile && (
                <Box className="menu" display={"flex"} alignItems={"center"}>
                  {MenuList.map((item) => (
                    <Box
                      key={item.text}
                      onClick={() => history.push(item.link)}
                    >
                      <Typography variant="h5" component="h5">
                        {item.text}
                      </Typography>
                    </Box>
                  ))}
                </Box>
              )}
              <Box ml={"20px"}>
                <AddressCard />
              </Box>
              {mobile && (
                <Hamburger
                  size={24}
                  distance={"lg"}
                  color={theme.palette.primary.main}
                  toggled={openMenu}
                  toggle={() => setOpenMenu(!openMenu)}
                />
              )}
            </Box>
          )}
        </Box>

        <Drawer
          className={cx(classes.drawerContainer)}
          anchor={"left"}
          open={openMenu}
          onClose={() => setOpenMenu(false)}
        >
          <Box className={cx(classes.drawer)}>
            {MenuList.map((item) => (
              <Box key={item.text} onClick={() => history.push(item.link)}>
                <Typography variant="h3" component="h3" key={item.text}>
                  {item.text}
                </Typography>
              </Box>
            ))}
          </Box>
        </Drawer>
      </Container>
    </Box>
  )
}

export default Header
