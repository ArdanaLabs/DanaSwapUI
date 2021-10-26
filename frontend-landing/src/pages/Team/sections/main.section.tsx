import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ReactPlayer from "react-player";
import ScrollAnimation from "react-animate-on-scroll";

import { useIsDarkMode } from "state/user/hooks";

const heroVideo =
  "https://background.sfo3.digitaloceanspaces.com/stablecoin/output.m3u8";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    position: "relative",
    "& video": {
      objectFit: "cover",
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
    background: "rgba(24, 34, 113, 0.6)",

    [breakpoints.down("xs")]: {
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

    "& > span": {
      color: palette.text.secondary,
    },

    [breakpoints.down("sm")]: {
      fontSize: "35px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "22px",
    lineHeight: "25px",
    width: "50%",
    color: palette.text.primary,
    marginTop: "30px",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
      width: "100%",
      marginTop: "15px",
      padding: "0px 10px",
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
      <ReactPlayer
        url={heroVideo}
        playing
        loop={true}
        muted
        width={!mobile ? "100%" : "unset"}
        height={"100vh"}
        playbackRate={0.5}
      />
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

export default MainSection;
