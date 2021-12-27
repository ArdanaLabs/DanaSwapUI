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
import ReactPlayer from "react-player"

import { useIsDarkMode } from "state/user/hooks"
import { GradientButton } from "components/Button"

import ICO_NEXT from "assets/icons/carousel-next.svg"
import ICO_PREV from "assets/icons/carousel-prev.svg"
import RoadMapImg from "assets/logos/roadmap.png"

const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/team/output.webm"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
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
        height: "500px",
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
    justifyContent: "center",
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
    lineHeight: "25px",
    width: "100%",
    marginTop: "30px",

    [breakpoints.down("xs")]: {
      lineHeight: "18.4px",
      marginTop: "15px",
      padding: "0px 10px",
    },
  },

  actionBar: {
    display: "flex",
    position: "absolute",
    left: "50%",
    bottom: "20px",
    transform: "translate(-50%, 0%)",
  },

  image: {
    position: "relative",
    lineHeight: 0,
    margin: "20px 10px",
  },

  photo: {
    position: "absolute",
    top: "50%",
    left: "50%",
    transform: "translate(-50%, -50%)",
    display: "inline-flex",
    justifyContent: "center",
    alignItems: "center",
    borderRadius: "50%",
    width: "25px",

    [breakpoints.down("xs")]: {
      width: "18px",
    },
  },
}))

interface Props {
  handleCarousel: (direction: number) => void
}

const HeroSection: React.FC<Props> = ({ handleCarousel }) => {
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
        height={"600px"}
        playbackRate={0.3}
      />
      <Box className={cx(classes.container)}>
        <Container>
          <Grid
            container
            spacing={2}
            alignItems="center"
            direction={!mobile ? "row" : "column-reverse"}
          >
            <Grid item xs={12} md={7}>
              <Typography
                variant="h3"
                component="h3"
                className={cx(classes.title)}
              >
                Roadmap
              </Typography>
              <Typography
                variant="h4"
                component="h4"
                className={cx(classes.content)}
              >
                Ardana is building the first All-in-One Stablecoin Ecosystem
                Built on Cardano. Learn more about our roadmap below.
              </Typography>
            </Grid>
            <Grid item xs={12} md={5}>
              <Box display="flex" justifyContent={"center"}>
                <img src={RoadMapImg} alt="roadmap" width="80%" />
              </Box>
            </Grid>
          </Grid>
        </Container>

        <Box className={cx(classes.actionBar)}>
          <Box className={cx(classes.image)} onClick={() => handleCarousel(-1)}>
            <GradientButton
              width={!mobile ? 75 : 50}
              height={!mobile ? 75 : 50}
            />
            <img className={cx(classes.photo)} src={ICO_PREV} alt="prev" />
          </Box>
          <Box className={cx(classes.image)} onClick={() => handleCarousel(1)}>
            <GradientButton
              width={!mobile ? 75 : 50}
              height={!mobile ? 75 : 50}
            />
            <img className={cx(classes.photo)} src={ICO_NEXT} alt="next" />
          </Box>
        </Box>
      </Box>
    </Box>
  )
}

export default HeroSection
