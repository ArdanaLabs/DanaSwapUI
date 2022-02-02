import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import { useIsDarkMode } from "state/user/hooks"
import { SocialBar } from "components"

import NewsHeroImage from "assets/logos/news-hero.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingTop: "100px",
    paddingBottom: "100px",

    [breakpoints.down("xs")]: {
      paddingTop: "100px",
      paddingBottom: "50px",
    },
  },
  title: {
    [`& > span`]: {
      color: palette.secondary.main,
    },
  },
}))

const HeroSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={classes.root}>
      <Container>
        <Grid container justifyContent="center">
          <Grid item xs={12} md={12}>
            <Grid
              container
              spacing={3}
              direction={!mobile ? "row" : "column-reverse"}
            >
              <Grid item container xs={12} md={6}>
                <Box
                  display="flex"
                  flexDirection="column"
                  justifyContent={"center"}
                  textAlign={!mobile ? "start" : "center"}
                >
                  <Typography
                    component="h3"
                    variant="h3"
                    className={classes.title}
                  >
                    Ardana <span>News</span>
                  </Typography>
                  <Box mb="10px" />
                  <Typography component="h4" variant="h4">
                    For the latest news, make sure to follow Ardana’s social
                    media accounts and keep checking back in to the Ardana
                    platform.
                  </Typography>
                  <Box mb="30px" />
                  <Box px={!mobile ? "0px" : "20px"}>
                    <SocialBar />
                  </Box>
                </Box>
              </Grid>
              <Grid item xs={12} md={6}>
                <Box textAlign="center">
                  <img src={NewsHeroImage} alt="hero" width={"100%"} />
                </Box>
              </Grid>
            </Grid>
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default HeroSection
