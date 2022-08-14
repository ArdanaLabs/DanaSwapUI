import React from "react"

import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/hero-bg-dark.png"
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/hero-bg-light.png"
import { Box, Container, Typography, Theme, useTheme } from "@mui/material"
import { makeStyles } from "@mui/styles"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    paddingTop: "150px",
    paddingBottom: "150px",
    background: `url(${
      theme.palette.mode === "dark"
        ? BACKGROUND_WAVE_BLUE
        : BACKGROUND_WAVE_WHITE
    }) right bottom no-repeat`,
    backgroundSize: "cover",
    borderBottomRightRadius: "100px",

    [theme.breakpoints.down("sm")]: {
      paddingTop: "100px",
      paddingBottom: "50px",
      textAlign: "center",
      background: `url(${
        theme.palette.mode === "dark"
          ? BACKGROUND_WAVE_BLUE
          : BACKGROUND_WAVE_WHITE
      }) right bottom no-repeat`,
      borderBottomRightRadius: "75px",
    },
  },

  title: {
    marginBottom: "30px",
    color: theme.palette.primary.main,
    textAlign: "center",
  },
}))

const HeroSection: React.FC = () => {
  const theme = useTheme()
  const classes = useStyles(theme)

  return (
    <Box className={classes.root}>
      <Container>
        <Typography variant="h1" component="h1" className={classes.title}>
          Your Vaults
        </Typography>
      </Container>
    </Box>
  )
}

export default HeroSection
