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
import BG_POSTER from "assets/backgrounds/video-poster.png";
import BG_LEFT from "assets/backgrounds/about-bg.png";
import BG_RIGHT from "assets/backgrounds/cyan-gradient.png";
import BG_PURPLE_GRADIENT from "assets/backgrounds/pink-gradient.png";
import { ArdanaFeatures } from "data";
import { GradientButton } from "components";

const aboutVideo =
  "https://background.sfo3.digitaloceanspaces.com/about/output.m3u8";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    width: "100%",
    background: ` url(${BG_LEFT}) top left no-repeat,
                  url(${BG_RIGHT}) bottom -200px right -500px no-repeat,
                  url(${BG_PURPLE_GRADIENT}) top -700px right -500px no-repeat`,
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

  content: {
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
      paddingLeft: "10px",
      paddingRight: "10px",
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
    position: "relative",
    lineHeight: 0,
    [breakpoints.down("xs")]: {
      margin: "60px",
    },
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
    [breakpoints.down("xs")]: {
      width: "60px",
    },
  }
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
                <Box className={cx(classes.title)}>
                  What is <span>Ardana</span>?
                </Box>
                <Box className={cx(classes.content)}>
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
                      <GradientButton
                        width={!mobile ? 145 : 81}
                        height={!mobile ? 145 : 81}
                        clickable={false}
                      />
                      <img className={cx(classes.photo)} src={ICON_PLAY} alt="playIcon" width="100px" />
                    </Box>
                  }
                />
              </Box>
            </Grid>
          </Grid>
        </Box>

        <Box mt={!mobile ? "200px" : "50px"}></Box>

        <Grid container spacing={3} alignItems="stretch">
          {ArdanaFeatures.map((feature, index) => (
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
