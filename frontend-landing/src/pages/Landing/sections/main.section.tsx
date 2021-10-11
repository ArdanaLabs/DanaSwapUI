import React from "react";
import { Box, useMediaQuery, Container, Grid, Link } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";
// import ReactPlayer from "react-player";

import { useIsDarkMode } from "state/user/hooks";

import { HeaderSection } from ".";
import { FeatureBox } from "components/Box";
import { GradientButton } from "components/Button";

import BG_PURPLE_RADIAL from "assets/backgrounds/purple-radial-gradient.png";
import BG_BLUE_RADIAL from "assets/backgrounds/dark-blue-radial-gradient.png";
import BG_WAVE from "assets/backgrounds/wave.png";
import LOGO_WHAT_IS_ARDANA from "assets/backgrounds/what-is-ardana.png";
import ICON_TWITTER from "assets/icons/twitter.svg";
import ICON_TELEGRAM from "assets/icons/telegram.svg";

// const sphereVideo = "assets/videos/sphere/output.m3u8";

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
    paddingBottom: "100px",
    background: ` url(${BG_PURPLE_RADIAL}) right top no-repeat,
                  url(${BG_BLUE_RADIAL}) top left no-repeat,
                  url(${BG_WAVE}) top 600px left no-repeat,
                  #080E42`,
    backgroundSize: "contain",
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "80px",
    lineHeight: "100%",
    color: "#F5FCFE",
    margin: "100px 0px 30px 0",

    "& > strong": {
      color: "#73D6F1",
    },

    [breakpoints.down("sm")]: {
      fontSize: "64px",
    },
  },

  subTitle: {
    color: "white",
    whiteSpace: "pre-line",
    fontSize: "24px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    lineHeight: "29px",

    [breakpoints.down("sm")]: {
      fontSize: "18px",
    },
  },

  definitionQ: {
    color: "#F5FCFE",
    fontSize: "80px",
    fontWeight: 900,
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    lineHeight: "200%",

    "& > .cyan": {
      color: "#73D6F1",
    },
  },

  definitionA: {
    color: "white",
    whiteSpace: "pre-line",
    fontWeight: 300,
    fontSize: "24px",
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    lineHeight: "30px",
  },

  socialIconLink: {
    marginRight: "50px",
    cursor: "pointer",
    color: "#F5FCFE",
    fontFamily: "auto",

    "& > img": {
      width: "23px",
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
        <HeaderSection />

        <Grid container>
          <Grid item xs={12} sm={6}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box
                className={cx(classes.title)}
                mr={!mobile && "-10px"}
                textAlign={mobile ? "center" : "left"}
                dangerouslySetInnerHTML={{
                  __html: i18next.t("PAGE.LANDING.TITLE", {
                    interpolation: { escapeValue: false },
                  }),
                }}
              />
              <Box
                className={cx(classes.subTitle)}
                mr={!mobile && "-150px"}
                textAlign={mobile ? "center" : "left"}
              >
                {i18next.t("PAGE.LANDING.DESCRIPTION")}
              </Box>
            </ScrollAnimation>

            <Box mt="50px"></Box>

            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box
                display="flex"
                alignItems="center"
                textAlign={mobile ? "center" : "left"}
              >
                <Link className={cx(classes.socialIconLink)} href="#">
                  <img src={ICON_TELEGRAM} alt="telegram" />
                </Link>
                <Link className={cx(classes.socialIconLink)} href="#">
                  <img src={ICON_TWITTER} alt="twitter" />
                </Link>
                <Link href="http://app.ardana.org/launch" underline="none">
                  <GradientButton
                    label={i18next.t("PAGE.LANDING.COMINGSOON")}
                    width={160}
                    height={40}
                  />
                </Link>
              </Box>
            </ScrollAnimation>
          </Grid>
          {/* <Grid item xs={12} sm={6}>
            <ReactPlayer
              url={sphereVideo}
              playing
              loop={true}
              muted
              width="100%"
              height="100%"
              playbackRate={0.5}
            />
          </Grid> */}
        </Grid>

        <Box mt={!mobile ? "250px" : "50px"}></Box>
        <Box>
          <Grid container spacing={1} alignItems="center">
            <Grid item xs={12} sm={6}>
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.definitionQ)}>
                  What is <span className="cyan">Ardana</span>?
                </Box>
                <Box className={cx(classes.definitionA)}>
                  {i18next.t("PAGE.LANDING.ARDANA.DESC")}
                </Box>
              </ScrollAnimation>
            </Grid>
            <Grid item xs={12} sm={6}>
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <img
                  src={LOGO_WHAT_IS_ARDANA}
                  alt="What is Ardana"
                  width="100%"
                />
              </ScrollAnimation>
            </Grid>
          </Grid>
        </Box>

        <Box mt={!mobile ? "150px" : "50px"}></Box>

        <Box>
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
                  custom_style={{
                    marginBottom: "50px",
                    padding: "20px",
                    flex: 2,
                    background: "#1D277A",
                  }}
                />
              </Grid>
            ))}
          </Grid>
        </Box>
      </Container>
    </Box>
  );
};

export default MainSection;
