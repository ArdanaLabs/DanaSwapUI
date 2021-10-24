import React from "react";
import { Box, useMediaQuery, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

import { useIsDarkMode } from "state/user/hooks";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    textAlign: "center",
    marginTop: "200px",
    marginBottom: "200px",
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "70px",
    lineHeight: "100%",
    color: "#73D6F1",

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
    color: "#F5FCFE",

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
