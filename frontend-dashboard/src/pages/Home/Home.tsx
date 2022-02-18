import React from "react"
import { Box, Fade, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import { useIsDarkMode } from "state/user/hooks"

import {
  OverViewSection,
  ChartSection,
  PoolsSection,
  StatsSection,
} from "./sections"

const useStyles = makeStyles(({ palette }) => ({
  root: {},
}))

const Home: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Fade in={true}>
      <Box className={classes.root}>
        <OverViewSection />

        <Box mt="50px" />

        <ChartSection />

        <Box mt="70px" />

        <PoolsSection />

        <Box mt="70px" />

        <StatsSection />
      </Box>
    </Fade>
  )
}

export default Home
