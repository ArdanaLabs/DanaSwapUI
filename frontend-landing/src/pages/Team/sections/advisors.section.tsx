import React from "react"
import { Box, useMediaQuery, Grid, Container } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { ProfileBox } from "components/Box"

import { Advisors } from "data"
import { ProfileType } from "components/Box/ProfileBox"

import BG_BLUE_RADIAL from "assets/backgrounds/cyan-gradient.png"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: ` url(${BG_BLUE_RADIAL}) top -300px left -1000px no-repeat,
                  url(${BG_BLUE_RADIAL}) bottom -300px right -1000px no-repeat`,
    padding: "0px 20px 100px 20px",

    [breakpoints.down("xs")]: {
      padding: "30px 0px 50px",
      background: `none`,
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontWeight: "bold",
    fontSize: "60px",
    lineHeight: "100%",
    color: palette.secondary.main,
    textAlign: "center",
    margin: "30px 0 100px",

    [breakpoints.down("xs")]: {
      fontSize: "35px",
      marginBottom: "0px",
    },
  },
  alignStretch: {
    display: "flex",
    alignItems: "stretch",
    flexFlow: "column",
  },
}))

const ProfileSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Box className={cx(classes.title)}>Advisors</Box>
        <Grid container spacing={!mobile ? 5 : 2}>
          {Advisors.map((profile: ProfileType, index) => (
            <Grid
              item
              key={index}
              xs={6}
              sm={4}
              md={3}
              className={cx(classes.alignStretch)}
            >
              <ProfileBox profile={profile} />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  )
}

export default ProfileSection
