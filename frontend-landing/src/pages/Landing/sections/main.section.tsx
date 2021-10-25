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
      paddingBottom: "100px",
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
                width={!mobile ? "300px" : "100%"}
                textAlign={!mobile ? "left" : "center"}
                flexDirection={!mobile ? "row" : "column-reverse"}
              >
                <Box
                  display="flex"
                  alignItems="center"
                  justifyContent="space-between"
                  width="100px"
                  mt={!mobile ? "0px" : "15px"}
                >
                  <Link className={cx(classes.socialIconLink)} href="#">
                    <TelegramIcon />
                  </Link>
                  <Link className={cx(classes.socialIconLink)} href="#">
                    <TwitterIcon />
                  </Link>
                </Box>
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
          <Grid item xs={12} sm={6}>
            <ReactPlayer
              url={sphereVideo}
              playing
              loop={true}
              muted
              width="100%"
              height="100%"
              playbackRate={0.5}
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default MainSection;
