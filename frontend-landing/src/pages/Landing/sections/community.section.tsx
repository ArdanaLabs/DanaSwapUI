import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"

import CommunityImage from "assets/logos/community.png"
import { CommunityBox, GradientBox } from "components"

const CommunityList = [
  {
    image: require("assets/logos/newspaper.png"),
    title: "Find out the\nlatest news",
    content:
      "Be up to the date on the latest Ardana updates  and announcements.",
    cta: {
      label: "Ardana Hub on Medium",
      link: "/",
      width: 220,
      height: 40,
    },
  },
  {
    image: require("assets/logos/communication-chat-bubble.png"),
    title: "Join the\nconversation",
    content: "Join our Telegram channel and Discord server.",
    cta: {
      label: "Follow us on Twitter",
      link: "/",
      width: 220,
      height: 40,
    },
  },
  {
    image: require("assets/logos/newspaper.png"),
    title: "Become an\nambassador",
    content: "Be part of the Team and become an Ardana ambassador.",
    cta: {
      label: "Coming Soon",
      link: "/",
      width: 160,
      height: 40,
    },
  },
]

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
              <GradientBox width={145} height={40}>
                <Typography component="div" variant="button">
                  Learn More
                </Typography>
              </GradientBox>
            </Box>
          </Grid>

          <Grid item xs={12} md={10}>
            <Box
              display={"flex"}
              justifyContent={"space-between"}
              alignItems={!mobile ? "stretch" : "center"}
              flexDirection={!mobile ? "row" : "column"}
            >
              {CommunityList.map((community) => (
                <CommunityBox key={community.title} {...community} />
              ))}
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default CommunitySection
