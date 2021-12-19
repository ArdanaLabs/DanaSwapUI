import { Box, Container, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import React from "react"
import { useIsDarkMode } from "state/user/hooks"

import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/hero-bg-dark.png"
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/hero-bg-light.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingTop: "150px",
    paddingBottom: "150px",
    background: `url(${
      palette.type === "dark" ? BACKGROUND_WAVE_BLUE : BACKGROUND_WAVE_WHITE
    }) right bottom no-repeat`,
    backgroundSize: "cover",
    borderBottomRightRadius: "100px",

    [breakpoints.down("xs")]: {
      paddingTop: "100px",
      paddingBottom: "50px",
      textAlign: "center",
      background: `url(${
        palette.type === "dark" ? BACKGROUND_WAVE_BLUE : BACKGROUND_WAVE_WHITE
      }) right bottom no-repeat`,
      borderBottomRightRadius: "75px",
    },
  },

  title: {
    marginBottom: "30px",
    color: palette.primary.main,
    textAlign: "center",
  },
}))

const HeroSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Typography variant="h1" component="h1" className={cx(classes.title)}>
          Your Vaults
        </Typography>
      </Container>
    </Box>
  )
}

export default HeroSection
