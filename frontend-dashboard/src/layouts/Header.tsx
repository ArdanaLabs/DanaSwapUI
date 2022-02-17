import React, { useState } from "react"
import {
  Box,
  IconButton,
  Drawer,
  useMediaQuery,
  Container,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import Hamburger from "hamburger-react"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { useHistory, useLocation } from "react-router-dom"
import ThemeSwitch from "components/ThemeSwitch"
import { GradientButton } from "components/Button"

import { navList } from "data"
import LogoLight from "assets/logo-light.png"
import LogoDark from "assets/logo-dark.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  self: {
    position: "fixed",
    top: 0,
    background: palette.background.default,
    zIndex: 100,
    width: "100%",
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

    [`&.active`]: {
      color: palette.text.secondary,
      [`&::before`]: {
        content: "' '",
        position: "absolute",
        top: "100%",
        width: "100%",
        left: 0,
        height: "2.5px",
        borderRadius: "2px",
        background: "linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)",

        [breakpoints.down("xs")]: {
          display: "none",
        },
      },
    },

    [breakpoints.down("xs")]: {
      margin: "10px",
    },
  },

  subHeader: {
    display: "flex",
    justifyContent: "flex-end",
    alignItem: "center",
    [breakpoints.down("xs")]: {
      marginTop: "30px",
    },
  },
}))

const Header: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("md"))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })
  const history = useHistory()
  const { pathname } = useLocation<{ previous: string }>()

  const [openMenu, setOpenMenu] = useState(false)

  const toggleMenu = () => {
    setOpenMenu((prev) => !prev)
  }

  const isActiveURL = (link: string): boolean => {
    return pathname.indexOf(link) > -1
  }

  const onConnectWallet = () => {
    console.log("connect wallet button clicked!")
  }

  return (
    <Box className={cx(classes.self)}>
      <Container>
        <Box className={cx(classes.container)}>
          <Box
            display="flex"
            justifyContent={!mobile ? "flex-start" : "space-between"}
            width="100%"
          >
            <Box className={cx(classes.logo)} onClick={() => history.push("/")}>
              <img src={dark ? LogoLight : LogoDark} alt="logo" />
            </Box>
            {!mobile && (
              <Box display="flex" ml="30px">
                {navList.map((navItem, index) => (
                  <Box
                    className={cx(classes.menuItem, {
                      active: isActiveURL(navItem.link),
                    })}
                    onClick={() => {
                      history.push(navItem.link)
                    }}
                    key={index}
                  >
                    {navItem.label}
                  </Box>
                ))}
              </Box>
            )}

            {mobile && (
              <>
                <IconButton
                  style={{ height: "60px", padding: 0 }}
                  onClick={() => setOpenMenu(!openMenu)}
                >
                  <Hamburger
                    size={30}
                    distance={"lg"}
                    color={"linear-gradient(90deg, #5F72FF 0%, #73D6F1 100%)"}
                    toggled={openMenu}
                    toggle={setOpenMenu}
                  />
                </IconButton>
                <Drawer anchor={"left"} open={openMenu} onClose={toggleMenu}>
                  <Box width={"50vw"}>
                    {navList.map((navItem, index) => (
                      <Box
                        key={index}
                        className={cx(classes.menuItem)}
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
          <Box className={cx(classes.subHeader)}>
            <ThemeSwitch />
            <GradientButton
              width={180}
              height={43}
              clickable
              label={"CONNECT WALLET"}
              gradientColor={
                dark ? ["#5F72FF", "#73D6F1"] : ["#030D53", "#3B4BC2"]
              }
              onClick={onConnectWallet}
            />
          </Box>
        </Box>
      </Container>
    </Box>
  )
}

export default Header
