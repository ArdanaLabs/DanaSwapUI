import React from "react";
import { Box, Container, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";

import COIN_DANA from "assets/image/COIN-DANA-3D.png";
import COIN_ETH from "assets/image/COIN-ETH-3D.png";
import COIN_HUOBI from "assets/image/COIN-HUOBI-3D.png";

import { TokenCard } from "components";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
}));

const AdSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={4}>
          <Grid item xs={12} sm={6} md={4}>
            <TokenCard
              name="LINK-A"
              label="NEW"
              stabilityFee={3}
              ratio={165}
              image={COIN_DANA}
              background={
                dark
                  ? "linear-gradient(180deg, #3142A3 0%, rgba(49, 66, 163, 0) 118.48%)"
                  : "linear-gradient(179.9deg, #0D1E7E 0.1%, #4D5EC1 99.98%)"
              }
            />
          </Grid>
          <Grid item xs={12} sm={6} md={4}>
            <TokenCard
              name="UNI-A"
              label="MOST POPULAR"
              stabilityFee={2}
              ratio={145}
              image={COIN_ETH}
              background={
                dark
                  ? "linear-gradient(180.2deg, #627EFF 0.17%, rgba(77, 97, 210, 0) 116.51%)"
                  : "linear-gradient(180deg, #2E49C5 0%, #6480FF 106.6%)"
              }
            />
          </Grid>
          <Grid item xs={12} sm={6} md={4}>
            <TokenCard
              name="GUSD-A"
              label="CHEAPEST"
              stabilityFee={0}
              ratio={101}
              image={COIN_HUOBI}
              background={
                dark
                  ? "linear-gradient(180deg, #71CEF3 0%, rgba(113, 206, 243, 0) 110%)"
                  : "linear-gradient(180deg, #0E70CA 0%, #64B2FA 110%)"
              }
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default AdSection;
