import React from "react"

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
    display: "flex",
    justifyContent: "center",
    alignItems: "center",
    height: "500px",
    paddingTop: "100px",
    paddingBottom: "50px",
    background: `url(${
      theme.palette.mode === "dark"
        ? BACKGROUND_WAVE_BLUE
        : BACKGROUND_WAVE_WHITE
    }) right bottom no-repeat`,
    backgroundSize: "cover",
    borderBottomRightRadius: "100px",

    [theme.breakpoints.down("sm")]: {
      height: "400px",
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

const AdSection: React.FC = () => {
  const theme = useTheme()
  const classes = useStyles(theme)

  return (
    <Box className={classes.root}>
      <Container>
        <Grid container justifyContent="center">
          <Grid item sm={12} md={10}>
            <Typography variant="h1" component="h1" className={classes.title}>
              Select a collateral type to create a Maker Vault and borrow Dai or
              buy additional collateral.
            </Typography>
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default AdSection
