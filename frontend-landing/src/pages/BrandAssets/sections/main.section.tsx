import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
// import ReactPlayer from "react-player";
import ScrollAnimation from "react-animate-on-scroll";

import { useIsDarkMode } from "state/user/hooks";

import BG_BRAND from "assets/backgrounds/brand.png";

// const heroVideo =
//   "https://background.sfo3.digitaloceanspaces.com/stablecoin/output.webm";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    position: "relative",
    // "& video": {
    //   objectFit: "cover",
    // },
    height: "400px",
    background: `url(${BG_BRAND}) top left no-repeat`,
  },
  container: {
    position: "absolute",
    top: 0,
    left: 0,
    width: "100%",
    height: "100%",
    display: "flex",
    alignItems: "center",
    background: "rgba(24, 34, 113, 0.6)",

    [breakpoints.down("xs")]: {
      padding: "50px",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "64px",
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
    fontSize: "18px",
    lineHeight: "25px",
    width: "50%",
    color: palette.text.primary,
    marginTop: "30px",

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
      width: "100%",
      marginTop: "15px",
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
      {/* <ReactPlayer
        url={heroVideo}
        playing={false}
        loop={true}
        muted
        width={!mobile ? "100%" : "unset"}
        height={"600px"}
        playbackRate={0.5}
      /> */}
      <Box className={cx(classes.container)}>
        <Container>
          <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
            <Box className={cx(classes.title)} mt="50px">
              Brand Assets
            </Box>
          </ScrollAnimation>
          <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
            <Box className={cx(classes.content)}>
              Our brand’s purpose is translated through our branding and
              designs. We have curated these assets with full instructions on
              how to properly utilise them.
            </Box>
          </ScrollAnimation>
        </Container>
      </Box>
    </Box>
  );
};

export default MainSection;
