import React from "react"
import { Box, Fade, useMediaQuery, Container } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"

import {
  OverViewSection,
  ChartSection,
  PoolsSection,
  StatsSection,
} from "./sections"

import CyanBG from "assets/backgrounds/cyan.svg"
import PinkBG from "assets/backgrounds/pink.svg"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: `url(${PinkBG}) right -600px top -600px no-repeat,
                  url(${CyanBG}) left -800px top -500px no-repeat`,
    paddingTop: "180px",
    paddingBottom: "50px",
  },
}))

const Home: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Fade in={true}>
      <Box className={classes.root}>
        <Container>
          <OverViewSection />

          <Box mt="50px" />

          <ChartSection />

          <Box mt="70px" />

          <PoolsSection />

          <Box mt="70px" />

          <StatsSection />
        </Container>
      </Box>
    </Fade>
  )
}

export default Home
