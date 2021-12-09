import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ReactPlayer from "react-player";
import ScrollAnimation from "react-animate-on-scroll";

import { useIsDarkMode } from "state/user/hooks";

// import BG_VECTEEZY from "assets/backgrounds/vecteezy.png";

const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/team/output.webm";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    position: "relative",
    "& video": {
      objectFit: "cover",
    },
  },
  background: {
    lineHeight: 0,

    "& > img": {
      width: "100%",
      height: "600px",

      [breakpoints.down("xs")]: {
        height: "400px",
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
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "60px",
    lineHeight: "100%",
    color: palette.text.primary,

    "& > span": {
      color: palette.text.secondary,
    },

    [breakpoints.down("xs")]: {
      fontSize: "35px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "16px",
    lineHeight: "25px",
    width: "50%",
    color: palette.text.primary,
    marginTop: "30px",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
      width: "100%",
      marginTop: "15px",
      padding: "0px 10px",
    },
  },
}));

const HeroSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

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
      {/* <Box className={cx(classes.background)}>
        <img src={BG_VECTEEZY} alt="bg" />
      </Box> */}
      <Box className={cx(classes.container)}>
        <Container>
          <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
            <Box className={cx(classes.title)} mt="50px">
              Ardana has a <br />
              <span>world class</span> team
            </Box>
          </ScrollAnimation>
          <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
            <Box className={cx(classes.content)}>
              Our team is comprised of technical talent, early contributors,
              students and ambassadors of reputable companies and blockchain
              projects such as Apple, Microsoft, Barclays, Citi Bank, State
              Street, Mina Protocol, Cardano, the Plutus Pioneers Program and
              Emurgo Academy.
            </Box>
          </ScrollAnimation>
        </Container>
      </Box>
    </Box>
  );
};

export default HeroSection;
