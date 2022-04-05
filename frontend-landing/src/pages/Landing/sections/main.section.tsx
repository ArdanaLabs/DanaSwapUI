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
// import ReactPlayer from "react-player"

import { useIsDarkMode } from "state/user/hooks"

import { GradientButton } from "components/Button"

import BG_PURPLE_RADIAL from "assets/backgrounds/pink-gradient.png"
import BG_BLUE_RADIAL from "assets/backgrounds/cyan-gradient.png"
import BG_WAVE from "assets/backgrounds/wave-gradient.png"
import BG_WAVE_MOBILE from "assets/backgrounds/wave-mobile-gradient.png"
import BG_HERO from "assets/backgrounds/hero.png"
import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg"
import { ReactComponent as TelegramIcon } from "assets/icons/telegram.svg"

import { Listings } from "data"

// const sphereVideo = "https://youtu.be/k_OaqxJaQCw"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: ` url(${BG_PURPLE_RADIAL}) top -700px right -700px no-repeat,
                  url(${BG_BLUE_RADIAL}) top 50% left 50% no-repeat,
                  url(${BG_WAVE}) bottom left no-repeat`,
    backgroundSize: "auto, auto, contain",
    paddingTop: "200px",
    paddingBottom: "200px",

    [breakpoints.down("xs")]: {
      background: ` url(${BG_PURPLE_RADIAL}) top -700px right -700px no-repeat,
                    url(${BG_BLUE_RADIAL}) top 50% left 50% no-repeat,
                    url(${BG_WAVE_MOBILE}) bottom left no-repeat`,
      backgroundSize: "auto, auto, 100%",
      textAlign: "center",
      paddingBottom: "130px",
    },
  },

  title: {
    lineHeight: "100%",

    [`& span`]: {
      color: palette.secondary.main,
    },

    [breakpoints.down("xs")]: {
      marginBottom: "10px",
    },
  },

  subTitle: {
    color: palette.primary.main,
    fontSize: "20px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    lineHeight: "26px",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
      whiteSpace: "pre-line",
    },
  },

  listingLabel: {
    fontFamily: "Museo Sans",
    fontSize: "12px",
    fontWeight: 100,
    lineHeight: "100%",
    color: palette.primary.main,
  },
  listingItem: {
    "& a": {
      "marginRight": "30px",

      "&:last-child": {
        marginRight: 0,
      },
    },
    "& img": {
      height: "35px",

      [breakpoints.down("xs")]: {
        height: "30px",
      },
    },
  },

  socialIconLink: {
    cursor: "pointer",
    color: palette.primary.main,

    [`&:hover path`]: {
      transition: "all .2s",
      fill: palette.secondary.main,
    },
  },
}))

const MainSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Grid
          container
          alignItems="center"
          direction={!mobile ? "row" : "column-reverse"}
        >
          <Grid item xs={12} sm={6}>
            <Box
              className={cx(classes.title)}
              textAlign={mobile ? "center" : "left"}
            >
              <Typography component="h1" variant="h1">
                Mint, Trade, Stake
                <br />
                and <span>Store Value</span>
              </Typography>
            </Box>
            <Box mt={"20px"} />
            <Box
              className={cx(classes.subTitle)}
              textAlign={mobile ? "center" : "left"}
            >
              The First All-in-One Stablecoin Ecosystem Built on Cardano
            </Box>

            <Box mt={!mobile ? "50px" : "30px"} />

            <Box
              display="flex"
              alignItems="center"
              justifyContent="space-between"
              width={!mobile ? "280px" : "100%"}
              textAlign={!mobile ? "left" : "center"}
              flexDirection={!mobile ? "row" : "column"}
            >
              {/* <Link href="http://app.ardana.org/launch" underline="none"> */}
              <GradientButton label={"TRADING NOW"} width={160} height={40} />
              {/* </Link> */}
              <Box
                display="flex"
                alignItems="center"
                justifyContent="space-between"
                width="80px"
                mt={!mobile ? "0px" : "15px"}
              >
                <Link
                  className={cx(classes.socialIconLink)}
                  href="https://t.me/ardanaofficial"
                  target="_blank"
                >
                  <TelegramIcon />
                </Link>
                <Link
                  className={cx(classes.socialIconLink)}
                  href="https://twitter.com/ardanaproject"
                  target="_blank"
                >
                  <TwitterIcon />
                </Link>
              </Box>
            </Box>

            <Box mt={!mobile ? "50px" : "30px"} />

            <Box className={cx(classes.listingLabel)}>LISTED ON:</Box>

            <Box mt={!mobile ? "30px" : "10px"} />

            <Box
              display="flex"
              alignItems="center"
              justifyContent={!mobile ? "flex-start" : "center"}
              className={cx(classes.listingItem)}
            >
              {Listings.map((item, index) => (
                <Link
                  key={index}
                  href={item.link}
                  target="_blank"
                  underline="none"
                >
                  <img src={item.image} alt="listing item" />
                </Link>
              ))}
            </Box>
          </Grid>
          <Grid item xs={12} sm={6} style={{ pointerEvents: "none" }}>
            {/* <ReactPlayer
              url={sphereVideo}
              playing
              loop={true}
              muted
              width="100%"
              height="100%"
              playbackRate={0.2}
            /> */}
            <img src={BG_HERO} alt="" />
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default MainSection
