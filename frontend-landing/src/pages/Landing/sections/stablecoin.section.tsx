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
// import ReactPlayer from "react-player";

import { useIsDarkMode } from "state/user/hooks"
import { GradientButton } from "components/Button"

import BG_VECTEEZY from "assets/backgrounds/vecteezy.png"
import IMG_STABLECOIN from "assets/logos/stablecoin-logo.png"

// const heroVideo =
//   "https://background.sfo3.digitaloceanspaces.com/team/output.webm";

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
      width: "100%",
      marginTop: "15px",
      padding: "0px 10px",
    },
  },
}))

const StableCoinSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      {/* <ReactPlayer
        url={heroVideo}
        playing={true}
        loop={true}
        muted
        width={!mobile ? "100%" : "unset"}
        height={!mobile ? "600px" : "400px"}
        playbackRate={0.3}
      /> */}
      <Box className={cx(classes.background)}>
        <img src={BG_VECTEEZY} alt="bg" />
      </Box>
      <Box className={cx(classes.container)}>
        <Container>
          <Grid container spacing={2} alignItems="center">
            <Grid item xs={12} sm={6}>
              <Box display="flex" justifyContent="center" alignItems={"center"}>
                <img alt="stablecoin" src={IMG_STABLECOIN} width={"80%"} />
              </Box>
            </Grid>
            <Grid item xs={12} sm={6}>
              <Typography
                component="h3"
                variant="h3"
                className={cx(classes.title)}
              >
                Stablecoin
              </Typography>
              <Typography
                variant="h4"
                component="h4"
                className={cx(classes.content)}
              >
                <span>dUSD</span> is verifiably backed by on-chain collateral
                and will enable borrowers to take leverage on their ADA or other
                supported assets.
              </Typography>
              <Box mt={!mobile ? "50px" : "30px"} />
              <GradientButton label={"COMING SOON"} width={160} height={40} />
            </Grid>
          </Grid>
        </Container>
      </Box>
    </Box>
  )
}

export default StableCoinSection
