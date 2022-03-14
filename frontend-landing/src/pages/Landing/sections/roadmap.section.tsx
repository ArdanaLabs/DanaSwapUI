import React from "react"
import {
  Box,
  useMediaQuery,
  Container,
  Grid,
  Link,
  Typography,
} from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import ReactPlayer from "react-player"

import { useIsDarkMode } from "state/user/hooks"
import { GradientButton } from "components/Button"

const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/team/output.webm"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    "padding": "0px",
    "position": "relative",
    "& video": {
      objectFit: "cover",
    },
  },
  background: {
    "lineHeight": 0,

    "& > img": {
      width: "100%",
      height: "600px",

      [breakpoints.down("xs")]: {
        height: "600px",
      },
    },
  },
  container: {
    position: "absolute",
    top: 0,
    left: 0,
    width: "100%",
    height: "100%",
    display: "flex",
    alignItems: "center",
    // background: "rgba(24, 34, 113, 0.6)",
    background:
      "linear-gradient(180deg, rgba(4, 13, 77, 0.7) -43.4%, rgba(50, 3, 111, 0.7) 222.51%)",

    [breakpoints.down("xs")]: {
      textAlign: "center",
    },
  },
  title: {
    lineHeight: "100%",
    color: palette.secondary.main,
  },

  content: {
    lineHeight: "26px",
    width: "100%",
    marginTop: "30px",

    [breakpoints.down("sm")]: {
      lineHeight: "18.4px",
      marginTop: "15px",
      padding: "0px 10px",
    },
  },
}))

const RoadMapSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <ReactPlayer
        url={heroVideo}
        playing={true}
        loop={true}
        muted
        width={!mobile ? "100%" : "unset"}
        height={!mobile ? "600px" : "400px"}
        playbackRate={0.3}
      />
      <Box className={cx(classes.container)}>
        <Container>
          <Grid container spacing={2} alignItems="center">
            <Grid item xs={12} sm={6}>
              <Typography
                variant="h3"
                component="h3"
                className={cx(classes.title)}
              >
                Roadmap
              </Typography>
              <Typography
                component="h4"
                variant="h4"
                className={cx(classes.content)}
              >
                Ardana is building the first All-in-One Stablecoin Ecosystem
                Built on Cardano. Learn more about our roadmap below.
              </Typography>
              <Box mt={!mobile ? "50px" : "30px"} />
              <Link href={"/roadmap"}>
                <GradientButton
                  label={"VIEW ROADMAP"}
                  width={160}
                  height={40}
                />
              </Link>
            </Grid>
          </Grid>
        </Container>
      </Box>
    </Box>
  )
}

export default RoadMapSection
