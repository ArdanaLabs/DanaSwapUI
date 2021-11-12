import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

import { useIsDarkMode } from "state/user/hooks";
import BG_PURPLE_RADIAL from "assets/backgrounds/pink-gradient.png";
import BG_BLUE_RADIAL from "assets/backgrounds/cyan-gradient.png";
import BG_CIRCLE from "assets/backgrounds/two-circle.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: ` url(${BG_PURPLE_RADIAL}) top -700px right -700px no-repeat,
                  url(${BG_BLUE_RADIAL}) top -500px left -800px no-repeat,
                  url(${BG_CIRCLE}) top left no-repeat`,
    paddingTop: "150px",
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "64px",
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
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={2}>
          <Grid item xs={12} sm={6}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.title)} mt="50px">
                Explorer
              </Box>
            </ScrollAnimation>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.content)}>
                Browse through our technical papers to learn more about the
                Ardana ecosystem.
              </Box>
            </ScrollAnimation>
          </Grid>

          <Grid item xs={12} sm={6}></Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default MainSection;
