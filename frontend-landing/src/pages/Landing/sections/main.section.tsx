import React from "react";
import { Box, useMediaQuery, Container, Grid, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";
import ReactPlayer from "react-player";

import { useIsDarkMode } from "state/user/hooks";

import { GradientButton } from "components/Button";

import BG_PURPLE_RADIAL from "assets/backgrounds/pink-gradient.png";
import BG_BLUE_RADIAL from "assets/backgrounds/cyan-gradient.png";
import BG_WAVE from "assets/backgrounds/wave-gradient.png";
import BG_WAVE_MOBILE from "assets/backgrounds/wave-mobile-gradient.png";
import { ReactComponent as TwitterIcon } from "assets/icons/twitter.svg";
import { ReactComponent as TelegramIcon } from "assets/icons/telegram.svg";

import { Listings } from "data";

const sphereVideo =
  "https://background.sfo3.digitaloceanspaces.com/sphere/output.webm";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: ` url(${BG_PURPLE_RADIAL}) top -700px right -700px no-repeat,
                  url(${BG_BLUE_RADIAL}) top 50% left 50% no-repeat,
                  url(${BG_WAVE}) bottom left no-repeat`,
    backgroundSize: "auto, auto, contain",
    paddingTop: "100px",
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
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "70px",
    lineHeight: "100%",
    color: palette.text.primary,
    whiteSpace: "pre-line",

    "& > strong": {
      color: palette.text.secondary,
    },

    [breakpoints.down("sm")]: {
      fontSize: "35px",
      marginBottom: "10px",
    },
  },

  subTitle: {
    color: palette.text.primary,
    fontSize: "22px",
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
    color: palette.text.primary,
  },
  listingItem: {
    "& img": {
      height: "35px",
      marginRight: "30px",

      [breakpoints.down("xs")]: {
        height: "30px",
      },

      "&:last-child": {
        marginRight: 0,
      },
    },
  },

  socialIconLink: {
    cursor: "pointer",
    color: palette.text.primary,

    "&:hover path": {
      fill: palette.text.secondary,
    },
  },
}));

const MainSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Grid
          container
          alignItems="center"
          direction={!mobile ? "row" : "column-reverse"}
        >
          <Grid item xs={12} sm={6}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box
                className={cx(classes.title)}
                textAlign={mobile ? "center" : "left"}
                dangerouslySetInnerHTML={{
                  __html: i18next.t("PAGE.LANDING.TITLE", {
                    interpolation: { escapeValue: false },
                  }),
                }}
              />
              <Box mt={"20px"} />
              <Box
                className={cx(classes.subTitle)}
                textAlign={mobile ? "center" : "left"}
              >
                {i18next.t("PAGE.LANDING.DESCRIPTION")}
              </Box>
            </ScrollAnimation>

            <Box mt={!mobile ? "50px" : "30px"} />

            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box
                display="flex"
                alignItems="center"
                justifyContent="space-between"
                width={!mobile ? "560px" : "100%"}
                textAlign={!mobile ? "left" : "center"}
                flexDirection={!mobile ? "row" : "column"}
              >
                {/* <Link href="http://app.ardana.org/launch" underline="none"> */}
                <GradientButton
                  label={
                    !mobile
                      ? "START TRADING ON NOVEMBER 22nd, 2021   |     2PM UTC"
                      : "START TRADING ON NOV 22nd, 2021 | 2PM UTC"
                  }
                  width={!mobile ? 426 : 350}
                  height={40}
                />
                {/* </Link> */}
                <Box
                  display="flex"
                  alignItems="center"
                  justifyContent="space-between"
                  width="100px"
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
            </ScrollAnimation>

            <Box mt={!mobile ? "50px" : "30px"} />

            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.listingLabel)}>LISTING ON:</Box>
            </ScrollAnimation>

            <Box mt={!mobile ? "30px" : "10px"} />

            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
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
            </ScrollAnimation>
          </Grid>
          <Grid item xs={12} sm={6}>
            <ReactPlayer
              url={sphereVideo}
              playing
              loop={true}
              muted
              width="100%"
              height="100%"
              playbackRate={0.2}
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default MainSection;
