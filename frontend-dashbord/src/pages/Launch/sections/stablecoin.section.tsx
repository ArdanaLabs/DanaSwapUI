import React from "react";
import { Box, Container, Grid, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import { useIsDarkMode } from "state/user/hooks";
import cx from "classnames";
import { StableCoinInfoBox } from "components/Box";

import IMG_TVL from "assets/icons/tvl.png";
import IMG_Worth from "assets/icons/worth.png";
import IMG_Ratio from "assets/icons/ratio.png";

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: "linear-gradient(180deg, #01062F 14.06%, #172271 100%)",
    position: "fixed",
    width: "100vw",
    height: "100vh",
    top: 0,
    left: 0,

    display: "flex",
    alignItems: "center",
  },

  carousel: {
    display: "flex",
    flexDirection: "column",
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    lineHeight: "120.5%",
    cursor: "pointer",

    "& > div": {
      margin: "30px 0px",
    },
    "& > div:first-child": {
      background: "-webkit-linear-gradient(-90deg, #FFFFFF, #ffffff00)",
      "-webkit-background-clip": "text",
      "-webkit-text-fill-color": "transparent",
    },
    "& > div:last-child": {
      background: "-webkit-linear-gradient(90deg, #FFFFFF, #ffffff00)",
      "-webkit-background-clip": "text",
      "-webkit-text-fill-color": "transparent",
    }
  },
  inactive: {
    fontSize: "30px",
    fontWeight: 300,
    lineHeight: "120.5%",
  },
  active: {
    fontSize: "45px",
    fontWeight: 900,
    lineHeight: "120.5%",
  },
}));

const StableCoinSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const dark = useIsDarkMode();
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Container style={{marginTop: "50px"}}>
        <Grid container>
          <Grid item xs={12} sm={6} md={4} className={cx(classes.carousel)}>
            <Box className={cx(classes.inactive)}>
              DANASWAP
            </Box>
            <Box className={cx(classes.active)}>
              ARDANA<br />STABLECOINS
            </Box>
            <Box className={cx(classes.inactive)}>
              MY DASHBOARD
            </Box>
          </Grid>
          <Grid container item xs={12} sm={6} md={8} spacing={3} alignContent="flex-end">
            <Grid item xs={4}>
              <StableCoinInfoBox
                image={IMG_TVL}
                title="TOTAL VALUE LOCKED"
                content="$1,234,567"
              />
            </Grid>
            <Grid item xs={4}>
              <StableCoinInfoBox
                image={IMG_Worth}
                title={`WORTH OF STABLECOINS\nIN CIRCULATION`}
                content="$998,654"
              />
            </Grid>
            <Grid item xs={4}>
              <StableCoinInfoBox
                image={IMG_Ratio}
                title="TOTAL COLLATERAL-TO-LOAN RATIO"
                content="1:1"
              />
            </Grid>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default StableCoinSection;
