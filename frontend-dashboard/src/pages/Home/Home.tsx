import React from "react"
import cx from "classnames"
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
  self: {},
}))

const Home: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Fade in={true}>
      <Box className={cx(classes.self)}>
        <OverViewSection />

        <Box mt="30px" />

        <ChartSection />

        <Box mt="50px" />

        <PoolsSection />

        <Box mt="50px" />

        <StatsSection />
      </Box>
    </Fade>
  )
}

export default Home
