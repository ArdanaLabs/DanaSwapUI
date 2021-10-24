import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

import { useIsDarkMode } from "state/user/hooks";

import BG_LEFT from "assets/backgrounds/danatoken-left-bg.png";
import BG_RIGHT from "assets/backgrounds/danatoken-right-bg.png";
import BG_CYAN_GRADIENT from "assets/backgrounds/cyan-gradient.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    textAlign: "center",
    background: ` url(${BG_LEFT}) top left no-repeat,
                  url(${BG_RIGHT}) top 50% right no-repeat,
                  url(${BG_CYAN_GRADIENT}) top 50% right -200px no-repeat`,
    backgroundSize: "auto 100vh, auto 50vh, 100vh",
    height: "100vh",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "70px",
    lineHeight: "100%",
    color: palette.text.secondary,

    [breakpoints.down("xs")]: {
      fontSize: "35px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "22px",
    lineHeight: "26px",
    color: palette.text.primary,

    [breakpoints.down("xs")]: {
      fontSize: "16px",
      lineHeight: "18.4px",
    },
  },
}));

const DanaswapSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Container maxWidth="md">
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.title)}>DANA Token</Box>
        </ScrollAnimation>
        <Box mt="20px" />
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.content)}>
            The DANA token is the utility and governance token of the Ardana
            ecosystem which rewards holders with profits from Ardana and allows
            those who hold it to vote on changes to the project’s parameters.
          </Box>
        </ScrollAnimation>
      </Container>
    </Box>
  );
};

export default DanaswapSection;
