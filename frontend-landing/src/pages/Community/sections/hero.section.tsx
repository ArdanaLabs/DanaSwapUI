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
import { CommunityList } from "data"
import { CommunityBox, SocialBar } from "components"

import CommunityHeroImage from "assets/logos/community-hero.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingTop: "150px",
    paddingBottom: "150px",
  },
  title: {
    color: palette.secondary.main,
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
          <Grid item xs={12} md={9}>
            <Grid
              container
              spacing={3}
              direction={!mobile ? "row" : "column-reverse"}
            >
              <Grid item xs={12} md={6}>
                <Box
                  display="flex"
                  flexDirection="column"
                  justifyContent={"center"}
                >
                  <Typography
                    component="h3"
                    variant="h3"
                    className={classes.title}
                  >
                    Community
                  </Typography>
                  <Box mb="10px" />
                  <Typography component="h4" variant="h4">
                    Get involved with our expansive community connecting you
                    with Ardana followers all throughout the globe.
                  </Typography>
                  <Box mb="30px" />
                  <SocialBar />
                </Box>
              </Grid>
              <Grid item xs={12} md={6}>
                <Box textAlign="center">
                  <img src={CommunityHeroImage} alt="hero" width={"100%"} />
                </Box>
              </Grid>
            </Grid>
          </Grid>

          <Grid item container xs={12} md={9} spacing={3} alignItems="stretch">
            {CommunityList.map((community) => (
              <Grid item key={community.title} xs={12} md={4}>
                <CommunityBox {...community} />
              </Grid>
            ))}
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default HeroSection
