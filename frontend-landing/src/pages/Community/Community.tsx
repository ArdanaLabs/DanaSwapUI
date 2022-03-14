import React from "react"
import { Box, makeStyles, useMediaQuery, useTheme } from "@material-ui/core"

import { HeroSection, TelegramSection } from "./sections"
import { useIsDarkMode } from "state/user/hooks"

import BG_PURPLE_GRADIENT from "assets/backgrounds/pink-gradient.png"
import BG_CYAN_GRADIENT from "assets/backgrounds/cyan-gradient.png"
import BG_COMMUNITY from "assets/backgrounds/community-bg.png"
import BG_COMMUNITY_MOBILE from "assets/backgrounds/community-bg-mobile.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: ` url(${BG_PURPLE_GRADIENT}) top -900px left -700px no-repeat,
                  url(${BG_CYAN_GRADIENT}) top -500px left -900px no-repeat,
                  url(${BG_CYAN_GRADIENT}) top 0px right -900px no-repeat,
                  url(${BG_PURPLE_GRADIENT}) bottom -400px left -900px no-repeat,
                  url(${BG_CYAN_GRADIENT}) bottom -900px right -900px no-repeat,
                  linear-gradient(180deg, rgba(4, 13, 77, 0.7) -43.4%, rgba(50, 3, 111, 0.7) 222.51%),
                  url(${BG_COMMUNITY}) top left no-repeat`,
    backgroundSize: "cover",

    [breakpoints.down("xs")]: {
      background: ` url(${BG_PURPLE_GRADIENT}) top -1800px left -1400px no-repeat,
                    linear-gradient(180deg, rgba(4, 13, 77, 0.7) -43.4%, rgba(50, 3, 111, 0.7) 222.51%),
                    url(${BG_COMMUNITY_MOBILE}) top left no-repeat`,
      backgroundSize: "cover",
    },
  },
}))

const Community: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={classes.root}>
      <HeroSection />
      <TelegramSection />
    </Box>
  )
}

export default Community
