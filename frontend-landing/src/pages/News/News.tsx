import React from "react"
import { Box, makeStyles, useMediaQuery, useTheme } from "@material-ui/core"

import {
  HeroSection,
  MediumSection,
  TwitterSection,
  MediaSection,
} from "./sections"
import { useIsDarkMode } from "state/user/hooks"

// import BG_PURPLE_GRADIENT from "assets/backgrounds/pink-gradient.png"
// import BG_CYAN_GRADIENT from "assets/backgrounds/cyan-gradient.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: ``,
    backgroundSize: "cover",

    [breakpoints.down("xs")]: {
      background: ``,
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
      <MediumSection />
      <TwitterSection />
      <MediaSection />
    </Box>
  )
}

export default Community
