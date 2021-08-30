import React from "react";
import { Box, useMediaQuery, Grid, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

import i18next from "i18next";
import { useIsDarkMode } from "state/user/hooks";

import MLABS from "assets/img/landing/logos/MLABS.svg";
import PSYSS from "assets/img/landing/logos/Platonic-Systems.svg";
import NODESEEDS from "assets/img/landing/logos/Nodeseeds.svg";
import OCCAM from "assets/img/landing/logos/Occam.svg";
import EMERGING from "assets/img/landing/logos/Emerging-Star.svg";
import DEFIRE from "assets/img/landing/logos/DeFire.svg";
import ISRAEL from "assets/img/landing/logos/Israel.svg";
import CRYPTODORMFUND from "assets/img/landing/logos/CryptoDormFund.svg";
import DELTAHUB from "assets/img/landing/logos/DeltaHub.svg";
import AU21 from "assets/img/landing/logos/AU21.svg";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "#FFFFFF",
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
            <Grid item xs={12} sm={3}>
              <Box className={cx(classes.partner)}>
                <img src={PSYSS} alt="Platonic Systems" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={3}>
              <Box className={cx(classes.partner)}>
                <img src={MLABS} alt="MLABS" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={3}>
              <Box className={cx(classes.partner)}>
                <img src={OCCAM} alt="Occam fi" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={3}>
              <Box className={cx(classes.partner)}>
                <img src={DEFIRE} alt="DEFIRE" />
              </Box>
            </Grid>
            <Grid item xs={12}>
              <Box className={cx(classes.partner)}>
                <img src={ISRAEL} alt="ISRAEL" />
              </Box>
            </Grid>
          </Grid>
        </ScrollAnimation>
      </Container>

      <Box mt="100px" />

      <Container>
        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Box className={cx(classes.title)}>
            {i18next.t("PAGE.LANDING.INVESTORS")}
          </Box>
        </ScrollAnimation>

        <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
          <Grid
            container
            style={{ padding: "20px" }}
            alignItems="center"
            spacing={3}
          >
            <Grid item xs={12} sm={4}>
              <Box className={cx(classes.partner)}>
                <img src={EMERGING} alt="Emerging Stars" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box className={cx(classes.partner)}>
                <img src={CRYPTODORMFUND} alt="CryptoDorumFund" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={4}>
              <Box className={cx(classes.partner)}>
                <img src={NODESEEDS} alt="NodeSeeds" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={6}>
              <Box className={cx(classes.partner)}>
                <img src={DELTAHUB} alt="DeltaHub" />
              </Box>
            </Grid>
            <Grid item xs={12} sm={6}>
              <Box className={cx(classes.partner)}>
                <img src={AU21} alt="AU21" />
              </Box>
            </Grid>
          </Grid>
        </ScrollAnimation>
      </Container>
    </Box>
  );
};

export default (PartnerSection);
