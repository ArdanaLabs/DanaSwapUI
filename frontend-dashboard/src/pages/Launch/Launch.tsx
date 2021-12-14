import React, { useEffect, useState } from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"
import cx from "classnames"
import { LaunchTotalStats, LaunchHeader, LaunchPartialStats } from "./sections"

import IMG_ScrollDown from "assets/icons/scroll-down.png"

const useStyles = makeStyles(({ palette }) => ({
  root: {},
  scroll: {
    position: "fixed",
    bottom: "10px",
    left: "calc(50vw - 30px)",
    cursor: "pointer",

    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "13px",
    lineHeight: "100%",
    textAlign: "center",
    color: "white",
  },
}))

const Launch: React.FC = () => {
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const dark = useIsDarkMode()
  const classes = useStyles({ dark, mobile })

  const [nav, setNav] = useState(1)

  const [curSection, setCurSection] = useState(0)

  const updateNav = (newNav: number) => {
    setNav(newNav)
  }

  const handleScrollDown = () => {
    curSection === 0 && setCurSection(1)
    curSection === 1 && setCurSection(0)
  }

  const handleScroll = (event: any) => {
    let direction: boolean = event.deltaY > 0
    direction && setCurSection(1)
    !direction && setCurSection(0)
  }

  useEffect(() => {
    window.addEventListener("wheel", (e) => handleScroll(e))
    // eslint-disable-next-line
  }, [])

  return (
    <Box className={cx(classes.root)}>
      <LaunchHeader nav={nav} updateNav={updateNav} />

      <LaunchTotalStats
        top={`-${(0 - curSection) * 100}vh`}
        show={curSection === 0}
      />
      <LaunchPartialStats
        top={`${(1 - curSection) * 100}vh`}
        show={curSection === 1}
      />

      <Box className={cx(classes.scroll)} onClick={handleScrollDown}>
        <img src={IMG_ScrollDown} alt="scroll down" />
        <br />
        <br />
        <Box>Scroll down</Box>
      </Box>
    </Box>
  )
}

export default Launch
