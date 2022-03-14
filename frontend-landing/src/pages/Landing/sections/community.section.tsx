import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
  Link,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { CommunityBox, GradientBox } from "components"
import { CommunityList } from "data"

import CommunityImage from "assets/logos/community.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
  title: {
    color: palette.secondary.main,
  },
}))

const CommunitySection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={3} justifyContent="center">
          <Grid item xs={12} md={6}>
            <Box
              display={"flex"}
              flexDirection={"column"}
              alignItems={"center"}
              textAlign="center"
            >
              <img src={CommunityImage} alt="community" />
              <Box mb="30px" />
              <Typography
                component="h3"
                variant="h3"
                className={cx(classes.title)}
              >
                Ardana Community
              </Typography>
              <Box mb="30px" />
              <Typography component="h4" variant="h4">
                Ardana is formed by people from various, different backgrounds.
                A whole community of developers, token holders and members. Led
                by a world-class team found all across the globe.
              </Typography>
              <Box mb="30px" />
              <Link href="/community">
                <GradientBox width={145} height={40}>
                  <Typography component="div" variant="button">
                    Learn More
                  </Typography>
                </GradientBox>
              </Link>
            </Box>
          </Grid>

          <Grid item container xs={12} md={9} spacing={3}>
            {CommunityList.map((community) => (
              <Grid item key={community.title} xs={12} md={4}>
                <CommunityBox {...community} short />
              </Grid>
            ))}
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default CommunitySection
