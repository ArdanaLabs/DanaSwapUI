import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Typography,
  Grid,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"

import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/hero-bg-dark.png"
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/hero-bg-light.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    height: "500px",
    paddingTop: "100px",
    paddingBottom: "50px",
    background: `url(${
      palette.type === "dark" ? BACKGROUND_WAVE_BLUE : BACKGROUND_WAVE_WHITE
    }) right bottom no-repeat`,
    backgroundSize: "cover",
    borderBottomRightRadius: "100px",

    [breakpoints.down("xs")]: {
      height: "400px",
      textAlign: "center",
      paddingBottom: "30px",
      background: `url(${
        palette.type === "dark" ? BACKGROUND_WAVE_BLUE : BACKGROUND_WAVE_WHITE
      }) right bottom no-repeat`,
      borderBottomRightRadius: "75px",
    },
  },

  title: {
    color: palette.primary.main,
    textAlign: "center",
  },
}))

const AdSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box
      className={cx(classes.root)}
      display={"flex"}
      justifyContent={"center"}
      alignItems={"center"}
    >
      <Container>
        <Grid container justifyContent="center">
          <Grid item sm={12} md={10}>
            <Typography
              variant="h1"
              component="h1"
              className={cx(classes.title)}
            >
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
