import React from "react";
import { Box, useMediaQuery, Grid, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

import i18next from "i18next";
import { useIsDarkMode } from "state/user/hooks";

import MLABS from "assets/img/landing/logos/MLABS.svg";
import PSYSS from "assets/img/landing/logos/Platonic-Systems.svg";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "#F5F5F5",
    padding: "100px 20px",

    [breakpoints.down("sm")]: {
      padding: "50px 0",
    },
  },
  title: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "64px",
    lineHeight: "100%",
    color: "#202F9A",
    textAlign: "center",

    [breakpoints.down("sm")]: {
      fontSize: "48px",
    },
  },
  partner: {
    textAlign: "center",

    "& img": {
      width: "100%",
      maxWidth: "max-content",
    },
  },
}));

const PartnerSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.title)}>
            {i18next.t("PAGE.LANDING.PARTNERS")}
          </Box>
        </ScrollAnimation>

        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Grid
            container
            style={{ padding: "20px" }}
            alignItems="center"
            spacing={3}
          >
            <Grid item xs={12} sm={6}>
              <Box className={cx(classes.partner)}>
                <img src={PSYSS} alt="Platonic Systems" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={6}>
              <Box className={cx(classes.partner)}>
                <img src={MLABS} alt="MLABS" />
              </Box>
            </Grid>
          </Grid>
        </ScrollAnimation>
      </Container>
    </Box>
  );
};

export default (PartnerSection);
