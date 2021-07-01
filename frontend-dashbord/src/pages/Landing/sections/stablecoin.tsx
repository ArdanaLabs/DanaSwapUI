import React from "react";
import { Box, useMediaQuery, Grid, Container } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

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
    title: "Ultra-low\nSlippage",
    content:
      "Unbiased, multi-collateral backed pegged to the US Dollar and other currencies.",
  },
  {
    image: img_savings_account,
    title: "Savings\nAccount",
    content:
      "Allow holders to borrow and lend the asset for use on exchanges like any other crypto asset.",
  },
  {
    image: img_earn_market_making_fees,
    title: "Earn Market\nMaking Fees",
    content:
      "Allow holders to borrow and lend the asset for use on exchanges like any other crypto asset.",
  },
  {
    image: img_foreign_exchange,
    title: "Foreign\nExchange",
    content:
      "Unbiased, multi-collateral backed pegged to the US Dollar and other currencies.",
  },
  {
    image: img_data_token,
    title: "DANA Token\n\n",
    content:
      "Allow holders to borrow and lend the asset for use on exchanges like any other crypto asset.",
  },
  {
    image: img_governance,
    title: "Governance\n\n",
    content:
      "Allow holders to borrow and lend the asset for use on exchanges like any other crypto asset.",
  },
];

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: "#000633",
    padding: "100px 20px 50px 20px",
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
    fontFamily: "Museo Sans",
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
        <Grid container alignItems="center">
          <Grid item xs={12} sm={4}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.title)}>Stablecoin</Box>
            </ScrollAnimation>
          </Grid>
          <Grid item xs={12} sm={4}>
            <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
              <Box className={cx(classes.content)}>
                The Ardana Dollar (dUSD) is a decentralized, unbiased, on-chain,
                collateral, backed cryptocurrency soft-pegged to the US dollar.
                dUSD is held in cryptocurrency wallets or within platforms, and
                is supported on Cardano.
              </Box>
            </ScrollAnimation>
          </Grid>
        </Grid>

        <Box mt="100px"></Box>

        <Grid container spacing={3}>
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
                    "linear-gradient(180deg, rgba(19, 27, 89, 0.5) 0%, #2F3DA0 100%)",
                }}
              />
            </Grid>
          ))}
        </Grid>
      </Container>
    </Box>
  );
};

export default StableCoinSection;
