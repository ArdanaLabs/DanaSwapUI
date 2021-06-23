import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

import { HeaderSection } from "./";

import LOGO_WITH_CITY from "assets/img/landing/logos/logo-with-city.png";
import LOGO_WITH_CITY_W from "assets/img/landing/logos/logo-with-city-white.png";
import { AdButton } from "components/Button";

import img_fully_decentralized from "assets/img/landing/icons/fully-decentralized.png";
import img_borrow_lend from "assets/img/landing/icons/borrow-lend.png";
import img_store_of_value from "assets/img/landing/icons/store-of-value.png";
import img_powered_by_cardano from "assets/img/landing/icons/powered-by-cardano.png";
import { FeatureBox } from "components/FeatureBox";

const Ardana_features = [
  {
    image: img_fully_decentralized,
    title: "Fully\nDecentralized",
    content: "Unbiased, multi-collateral backed pegged to the US Dollar and other currencies.",
  },
  {
    image: img_borrow_lend,
    title: "Borrow\n& Lend",
    content: "Allow holders to borrow and lend the asset for use on exchanges like any other crypto asset.",
  },
  {
    image: img_store_of_value,
    title: "Store of\nValue",
    content: "Designed to function as a secure store of value that accrues value even in a volatile market.",
  },
  {
    image: img_powered_by_cardano,
    title: "Powered by\nCardano",
    content: "Ardana stablecoins are designed to function as a store of value even in a volatile market.",
  },
]

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    background: `url(${LOGO_WITH_CITY}) right top no-repeat, url(${LOGO_WITH_CITY_W}) right top no-repeat, linear-gradient(90.19deg, #2F3DA0 0.2%, #73D6F1 80.66%)`,
    paddingBottom: "50px",
  },
}));

const MainSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.bg)}>
      <Container>
        <HeaderSection />

        <Box
          component="p"
          textAlign={"center"}
          pt={"30px"}
          color={"white"}
          whiteSpace={"pre-line"}
          fontWeight={900}
          fontSize={!mobile ? "100px" : "2.8em"}
          m={0}
        >
          {"A decentralized\nstablecoin hub."}
        </Box>

        <Box
          component="p"
          textAlign={"center"}
          color={"white"}
          whiteSpace={"pre-line"}
          fontWeight={500}
          fontSize={!mobile ? "24px" : "1.1em"}
          m={0}
        >
          {"Decentralized Stablecoin and DEX Liquidity Pool. Built on Cardano."}
        </Box>

        <Box mt="50px"></Box>
        <Box textAlign="center">
          <AdButton variant="contained">BUY TOKEN</AdButton>
          <AdButton variant="contained">LAUNCH PLATFORM</AdButton>
        </Box>

        <Box mt={!mobile ? "100px" : "50px"}></Box>
        {/* <Box>
          <Grid container spacing={1}>
            <Grid item xs={12} sm={6}>
              <Box
                color="white"
                fontSize={!mobile ? "64px" : "48px"}
                fontWeight="900"
              >
                What is Ardana?
              </Box>
            </Grid>
            <Grid item xs={12} sm={6}>
              <Box
                color="white"
                whiteSpace="pre-line"
                fontWeight="300"
                fontSize="18px"
                lineHeight="27px"
              >
                Ardana is a decentralized financial hub and services provider
                built on Cardano that provides key DeFi primitives including a
                decentralized stablecoin exchange, stablecoins, a foreign
                exchange protocol and a multisignature protocol to power users
                and open finance applications on Cardano and beyond.
              </Box>
            </Grid>
          </Grid>
        </Box>

        <Box mt="100px"></Box>
        
        <Box>
          <Grid container spacing={3} alignItems="stretch">
            {
              Ardana_features.map((feature, index) => (
                <Grid item key={index} xs={12} sm={6} md={3} style={{display: "flex", alignItems: "stretch", flexFlow: "column"}}>
                  <FeatureBox image={feature.image} title={feature.title} content={feature.content} custom_style={{padding: "20px", background: "linear-gradient(180deg, rgba(115, 214, 241, 0) 0%, #2F3DA0 100%)", flex: 2}} />
                </Grid>
              ))
            }
          </Grid>
        </Box> */}

        <Box mt="30px"></Box>
      </Container>
    </Box>
  );
};

export default MainSection;
