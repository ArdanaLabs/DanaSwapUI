import React from "react"
import cx from "classnames"
import { Box, Fade, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
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
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  return (
    <Fade in={true}>
      <Box className={cx(classes.self)}>
        <OverViewSection />

        {/* TODO: don’t use <div>s for spacing; use padding, gap, etc. */}
        <Box mt="30px" />

        <ChartSection />

        {/* TODO: don’t use <div>s for spacing; use padding, gap, etc. */}
        <Box mt="50px" />

        <PoolsSection />

        {/* TODO: don’t use <div>s for spacing; use padding, gap, etc. */}
        <Box mt="50px" />

        <StatsSection />
      </Box>
    </Fade>
  )
}

export default Home
