import React from "react";
import { Box, useMediaQuery, Grid, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";
import i18next from "i18next";

import { useIsDarkMode } from "state/user/hooks";

import img_ultra_low_slippage from "assets/img/landing/icons/ultra-low-slippage.png";
import img_savings_account from "assets/img/landing/icons/savings-account.png";
import img_earn_market_making_fees from "assets/img/landing/icons/earn-market-making-fees.png";
import img_foreign_exchange from "assets/img/landing/icons/foreign-exchange.png";
import img_data_token from "assets/img/landing/icons/data-token.png";
import img_governance from "assets/img/landing/icons/governance.png";
import { FeatureBox } from "components/Box";

const StableCoin_features = [
  {
    image: img_ultra_low_slippage,
    title: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.0.TITLE"),
    content: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.0.CONTENT"),
  },
  {
    image: img_savings_account,
    title: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.1.TITLE"),
    content: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.1.CONTENT"),
  },
  {
    image: img_earn_market_making_fees,
    title: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.2.TITLE"),
    content: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.2.CONTENT"),
  },
  {
    image: img_foreign_exchange,
    title: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.3.TITLE"),
    content: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.3.CONTENT"),
  },
  {
    image: img_data_token,
    title: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.4.TITLE"),
    content: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.4.CONTENT"),
  },
  {
    image: img_governance,
    title: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.5.TITLE"),
    content: i18next.t("PAGE.LANDING.STABLECOIN.FEATURES.5.CONTENT"),
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "linear-gradient(119.89deg, #2F3DA0 52.83%, #73D6F1 103.68%)",
    padding: "100px 20px",
  },
  title: {
    fontFamily: "Brandon Grotesque Bold",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "64px",
    lineHeight: "100%",
    color: "#FFFFFF",

    [breakpoints.down("sm")]: {
      fontSize: "48px",
    },
  },

  content: {
    fontFamily: "'Museo Sans 300'",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "18px",
    lineHeight: "22px",
    color: "#FFFFFF",

    [breakpoints.down("sm")]: {
      fontSize: "16px",
    },
  },
}));

const StableCoinSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <Box mt={!mobile ? "50px" : "0px"}></Box>
        <Grid container alignItems="center">
          <Grid item xs={12} sm={4}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.title)}>
                {i18next.t("PAGE.LANDING.STABLECOIN.TITLE")}
              </Box>
            </ScrollAnimation>
          </Grid>
          <Grid item xs={12} sm={4}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.content)}>
                {i18next.t("PAGE.LANDING.STABLECOIN.CONTENT")}
              </Box>
            </ScrollAnimation>
          </Grid>
        </Grid>

        <Box mt={!mobile ? "150px" : "50px"}></Box>

        <Grid container spacing={1}>
          {StableCoin_features.map((feature, index) => (
            <Grid
              item
              key={index}
              xs={12}
              sm={6}
              md={4}
              style={{
                display: "flex",
                alignItems: "stretch",
                flexFlow: "column",
              }}
            >
              <FeatureBox
                image={feature.image}
                title={feature.title}
                content={feature.content}
                custom_style={{
                  marginBottom: "70px",
                  padding: "40px",
                  flex: 2,
                  background:
                    "linear-gradient(180deg, #2F3DA0 0%, #73D6F1 100%)",
                }}
              />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  );
};

export default (StableCoinSection);
