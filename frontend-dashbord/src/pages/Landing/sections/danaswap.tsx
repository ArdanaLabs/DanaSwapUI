import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

import img_Ellipse from "assets/img/landing/backgrounds/ellipse.png";
import img_Danaswap from "assets/img/landing/backgrounds/dana-swap-graphic.png";

const useStyles = makeStyles(({ palette }) => ({
  bg: {
    background: `url(${img_Ellipse}) calc(100% - 30px) calc(100% - 30px) no-repeat, #FFFFFF`,
    padding: "50px",
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
            <Box style={!mobile ? {padding: "20px 100px 20px"} : {padding: "0"}}>
              <Box
                fontSize={!mobile ? "64px" : "48px"}
                fontWeight="900"
                color="#202F9A"
              >
                Danaswap
              </Box>
              <Box
                fontSize={"18px"}
                fontWeight="300"
                color="#A5A5A5"
                lineHeight="27px"
              >
                Ardana is a decentralized financial hub and services provider
                built on Cardano that provides key DeFi primitives including a
                decentralized stablecoin exchange, stablecoins, a foreign
                exchange protocol and a multisignature protocol to power users
                and open finance applications on Cardano and beyond.
              </Box>
            </Box>
          </Grid>
          
          <Grid item xs={12} sm={6}>
            <img src={img_Danaswap} alt="Danaswap" width="100%" />
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default DanaswapSection;
