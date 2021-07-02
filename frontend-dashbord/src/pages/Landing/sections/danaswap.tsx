import React from "react";
import { Box, useMediaQuery, Container, Grid } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";
import ScrollAnimation from "react-animate-on-scroll";

import { useIsDarkMode } from "state/user/hooks";

import img_Ellipse from "assets/img/landing/backgrounds/ellipse.png";
import img_Danaswap from "assets/img/landing/backgrounds/dana-swap-graphic.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  bg: {
    background: `url(${img_Ellipse}) calc(100% - 30px) calc(100% - 30px) no-repeat, #FFFFFF`,
    padding: "50px",
  },

  title: {
    fontFamily: "Brandon Grotesque Bold",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "64px",
    lineHeight: "100%",
    color: "#202F9A",

    [breakpoints.down("sm")]: {
      fontSize: "48px",
    },
  },

  content: {
    fontFamily: "'Museo Sans 300'",
    fontStyle: "normal",
    fontWeight: 300,
    fontSize: "18px",
    lineHeight: "150%",
    color: "#A5A5A5",
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
            <Box
              style={
                !mobile ? { padding: "20px 100px 20px" } : { padding: "0" }
              }
            >
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.title)}>Danaswap</Box>
              </ScrollAnimation>
              <Box mt="20px" />
              <ScrollAnimation animateIn="fadeInUp" animateOnce={true}>
                <Box className={cx(classes.content)}>
                  Ardana is a decentralized financial hub and services provider
                  built on Cardano that provides key DeFi primitives including a
                  decentralized stablecoin exchange, stablecoins, a foreign
                  exchange protocol and a multisignature protocol to power users
                  and open finance applications on Cardano and beyond.
                </Box>
              </ScrollAnimation>
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
