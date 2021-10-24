import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";
import ReactPlayer from "react-player";

import { useIsDarkMode } from "state/user/hooks";

import { FeatureBox } from "components/Box";

import ICON_PLAY from "assets/icons/video-play.svg";
import BG_POSTER from "assets/backgrounds/video-poster.svg";
import BG_LEFT from "assets/backgrounds/about-bg.png";
import BG_RIGHT from "assets/backgrounds/cyan-gradient.png";

const aboutVideo =
  "https://background.sfo3.digitaloceanspaces.com/about/output.m3u8";

const Ardana_features = [
  {
    image: require("assets/logos/fully-decentralized.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.0.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.0.CONTENT"),
  },
  {
    image: require("assets/logos/borrow-lend.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.1.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.1.CONTENT"),
  },
  {
    image: require("assets/logos/store-of-value.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.2.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.2.CONTENT"),
  },
  {
    image: require("assets/logos/powered-by-cardano.svg").default,
    title: i18next.t("PAGE.LANDING.ARDANA.FEATURES.3.TITLE"),
    content: i18next.t("PAGE.LANDING.ARDANA.FEATURES.3.CONTENT"),
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    width: "100%",
    background: ` url(${BG_LEFT}) top left no-repeat,
                  url(${BG_RIGHT}) bottom -200px right -500px no-repeat`,
    backgroundSize: "contain",
    paddingTop: "100px",
    paddingBottom: "200px",

    [breakpoints.down("xs")]: {
      paddingBottom: "100px",
      backgroundPositionY: "250px, 1000px",
      textAlign: "center",
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

  definitionQ: {
    color: palette.text.primary,
    fontSize: "70px",
    fontWeight: 900,
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    lineHeight: "100%",
    marginBottom: "30px",

    "& > span": {
      color: palette.text.secondary,
    },

    [breakpoints.down("xs")]: {
      fontSize: "35px",
      marginBottom: "15px",
    },
  },

  definitionA: {
    color: palette.text.primary,
    whiteSpace: "pre-line",
    fontWeight: 300,
    fontSize: "22px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    lineHeight: "26px",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },

  socialIconLink: {
    cursor: "pointer",
    color: palette.text.primary,

    "&:hover path": {
      fill: palette.text.secondary,
    },
  },

  aboutVideo: {
    "& > div > video, & .react-player__preview": {
      borderRadius: "10px",
    },
    [breakpoints.down("xs")]: {
      marginTop: "20px",
      marginLeft: "-16px",
      marginRight: "-16px",
    },
  },

  playIcon: {
    margin: "100px",
    [breakpoints.down("xs")]: {
      margin: "60px",
    },
  },
}));

const AboutSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={classes.bg}>
      <Container>
        <Box>
          <Grid container spacing={3} alignItems="center">
            <Grid item xs={12} sm={6}>
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.definitionQ)}>
                  What is <span>Ardana</span>?
                </Box>
                <Box className={cx(classes.definitionA)}>
                  {i18next.t("PAGE.LANDING.ARDANA.DESC")}
                </Box>
              </ScrollAnimation>
            </Grid>
            <Grid item xs={12} sm={6}>
              <Box className={cx(classes.aboutVideo)}>
                <ReactPlayer
                  url={aboutVideo}
                  playing
                  loop={false}
                  width="100%"
                  height="100%"
                  controls
                  light={BG_POSTER}
                  playIcon={
                    <Box className={cx(classes.playIcon)}>
                      <img src={ICON_PLAY} alt="playIcon" width="100px" />
                    </Box>
                  }
                />
              </Box>
            </Grid>
          </Grid>
        </Box>

        <Box mt={!mobile ? "200px" : "50px"}></Box>

        <Grid container spacing={3} alignItems="stretch">
          {Ardana_features.map((feature, index) => (
            <Grid
              item
              key={index}
              xs={12}
              sm={6}
              md={3}
              style={{
                display: "flex",
                alignItems: "stretch",
                flexFlow: "column",
              }}
            >
              <FeatureBox
                image={feature.image}
                title={feature.title}
                content={feature.content}
              />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  );
};

export default AboutSection;
