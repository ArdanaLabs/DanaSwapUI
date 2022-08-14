import React from "react"
import { useParams } from "react-router-dom"
import {
  Box,
  Container,
  Grid,
  Theme,
  Typography,
  useTheme,
} from "@mui/material"
import { makeStyles } from "@mui/styles"

import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/hero-bg-dark.png"
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/hero-bg-light.png"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    height: "300px",
    paddingTop: "100px",
    paddingBottom: "50px",
    background: `url(${
      theme.palette.mode === "dark"
        ? BACKGROUND_WAVE_BLUE
        : BACKGROUND_WAVE_WHITE
    }) right bottom no-repeat`,
    backgroundSize: "cover",
    borderBottomRightRadius: "100px",
    display: "flex",
    justifyContent: "center",
    alignItems: "center",

    [theme.breakpoints.down("sm")]: {
      height: "200px",
      textAlign: "center",
      paddingBottom: "30px",
      background: `url(${
        theme.palette.mode === "dark"
          ? BACKGROUND_WAVE_BLUE
          : BACKGROUND_WAVE_WHITE
      }) right bottom no-repeat`,
      borderBottomRightRadius: "75px",
    },
  },

  title: {
    color: theme.palette.primary.main,
    textAlign: "center",
  },
}))

const HeroSection: React.FC = () => {
  const theme = useTheme()
  const classes = useStyles(theme)

  const { type } = useParams<{ type: string }>()

  return (
    <Box className={classes.root}>
      <Container>
        <Grid container justifyContent="center">
          <Grid item sm={12} md={10}>
            <Typography variant="h1" component="h1" className={classes.title}>
              Open {type.toUpperCase()} Vault
            </Typography>
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default HeroSection
