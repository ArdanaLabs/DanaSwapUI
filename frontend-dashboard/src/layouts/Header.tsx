import React, { useState, useEffect } from "react"
import {
  Box,
  IconButton,
  Drawer,
  useMediaQuery,
  Container,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import Hamburger from "hamburger-react"
import cx from "classnames"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
import { useHistory, useLocation } from "react-router-dom"
import ThemeSwitch from "components/ThemeSwitch"
import { GradientBox } from "components"

import { navList } from "data"
import LogoLight from "assets/logo-light.png"
import LogoDark from "assets/logo-dark.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "fixed",
    top: 0,
    zIndex: 200,
    width: "100%",
    filter: "drop-shadow(0px 15px 15px rgba(0, 0, 0, 0.05))",
    mixBlendMode: "normal",
  },

  container: {
    display: "flex",
    justifyContent: "space-between",
    alignItems: "center",
    transition: "background .2s ease-in",
    padding: "30px 0px",

    [breakpoints.down("xs")]: {
      flexDirection: "column",
    },
  },

  nav: {
    display: "flex",
    justifyContent: "flex-start",
    width: "100%",

    [breakpoints.down("xs")]: {
      justifyContent: "space-between",
    },
  },

  logo: {
    paddingLeft: "10px",
    display: "flex",
    alignItems: "center",
    cursor: "pointer",
    [`& img`]: {
      height: "45px",
      marginRight: "10px",
    },
  },

  menuItem: {
    fontFamily: "Museo Sans",
    fontWeight: 900,
    fontStyle: "normal",
    lineHeight: "100%",
    margin: "auto 20px",
    padding: "8px 0px",
    color: palette.text.primary,
    fontSize: "13px",
    position: "relative",
    textAlign: "center",
    cursor: "pointer",
    textTransform: "uppercase",

    [`&:hover`]: {
      color: palette.text.secondary,
    },

    [`&.isActive`]: {
      color: palette.text.secondary,
      [`&::before`]: {
        content: "' '",
        position: "absolute",
        top: "100%",
        width: "100%",
        left: 0,
        height: "2.5px",
        borderRadius: "2px",
        background: `linear-gradient(90deg, ${palette.secondary.dark} 0%, ${palette.secondary.main} 100%)`,

        [breakpoints.down("xs")]: {
          display: "none",
        },
      },
    },

    [breakpoints.down("xs")]: {
      margin: "10px",
    },
  },

  cta: {
    display: "flex",
    justifyContent: "flex-end",
    alignItem: "center",
    [breakpoints.down("xs")]: {
      marginTop: "30px",
    },
  },

  connectWallet: {
    color: palette.primary.main,
    textTransform: "uppercase",
    whiteSpace: "pre",
  },
}))

const Header: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))
  const userTheme: Theme.Theme = useUserTheme()
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })
  const isDarkTheme: boolean = Theme.Eq.equals(userTheme, Theme.Theme.Dark)
  const history = useHistory()
  const { pathname } = useLocation<{ previous: string }>()

  const [openMenu, setOpenMenu] = useState(false)
  const [bgColor, setBGColor] = useState("transparent")

  const toggleMenu = () => {
    setOpenMenu((prev) => !prev)
  }

  const isActiveURL = (link: string): boolean => {
    return pathname.indexOf(link) > -1
  }

  const onConnectWallet = (_event: any) => {
    console.log("connect wallet button clicked!")
  }

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
    <Box className={classes.root} style={{ background: bgColor }}>
      <Container>
        <Box className={classes.container}>
          <Box className={classes.nav}>
            <Box className={classes.logo} onClick={() => history.push("/")}>
              <img src={isDarkTheme ? LogoLight : LogoDark} alt="" />
            </Box>

            {!mobile ? (
              <Box display="flex" ml="30px">
                {navList.map((navItem, index) => (
                  <Box
                    className={cx(classes.menuItem, {
                      isActive: isActiveURL(navItem.link),
                    })}
                    onClick={() => history.push(navItem.link)}
                    key={index}
                  >
                    {navItem.label}
                  </Box>
                ))}
              </Box>
            ) : (
              <>
                <IconButton
                  style={{ height: "48px", padding: 0 }}
                  onClick={() => setOpenMenu(!openMenu)}
                >
                  <Hamburger
                    size={30}
                    distance={"lg"}
                    color={`linear-gradient(90deg, ${theme.palette.secondary.dark} 0%, ${theme.palette.secondary.main} 100%)`}
                    toggled={openMenu}
                    toggle={setOpenMenu}
                  />
                </IconButton>
                <Drawer anchor={"left"} open={openMenu} onClose={toggleMenu}>
                  <Box width={"50vw"}>
                    {navList.map((navItem, index) => (
                      <Box
                        key={index}
                        className={classes.menuItem}
                        onClick={() => {
                          history.push(navItem.link)
                        }}
                      >
                        {navItem.label}
                      </Box>
                    ))}
                  </Box>
                </Drawer>
              </>
            )}
          </Box>
          <Box className={classes.cta}>
            <ThemeSwitch />
            <GradientBox onClick={onConnectWallet}>
              <Typography
                variant="body2"
                component="span"
                className={classes.connectWallet}
              >
                Connect Wallet
              </Typography>
            </GradientBox>
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Header
