import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";
import { AdButton } from "components/Button";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "#080E42",
    padding: "200px 0px",

    [breakpoints.down("sm")]: {
      padding: "50px",
    },
  },

  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "80px",
    lineHeight: "100%",
    color: "#73D6F1",

    [breakpoints.down("sm")]: {
      fontSize: "48px",
    },
  },

  content: {
    fontFamily: "Museo Sans",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "25px",
    lineHeight: "30px",
    color: "#F5FCFE",
  },
}));

const DanaswapSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Grid container alignItems="center" spacing={3}>
          <Grid item xs={12} sm={6}>
            <Box>
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.title)}>
                  {i18next.t("PAGE.LANDING.DANASWAP.TITLE")}
                </Box>
              </ScrollAnimation>
              <Box mt="20px" />
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.content)}>
                  {i18next.t("PAGE.LANDING.DANASWAP.CONTENT")}
                </Box>
              </ScrollAnimation>
              <Box mt="50px" />
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <AdButton variant="contained">
                  {i18next.t("PAGE.LANDING.DANASWAP.BUTTON")}
                </AdButton>
              </ScrollAnimation>
            </Box>
          </Grid>

          <Grid item xs={12} sm={6}></Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default DanaswapSection;
