import React from "react";
import { Box, Grid, Container, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { ThemeSwitch } from "components";

import BACKGROUND_GRAPHIC from "assets/image/backgrounds/BG-GRAPHIC.png";
import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/BG-BLUE.png";
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/BG-WHITE.png";
import BACKGROUND_WAVE_BLUE_M from "assets/image/backgrounds/BG-BLUE-MOBILE.png";
import BACKGROUND_WAVE_WHITE_M from "assets/image/backgrounds/BG-WHITE-MOBILE.png";
import COIN_CARDANO from "assets/image/COIN1.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    paddingTop: "100px",
    paddingBottom: "50px",
    background: `url(${
      palette.type === "dark" ? BACKGROUND_WAVE_BLUE : BACKGROUND_WAVE_WHITE
    }) no-repeat`,
    backgroundSize: "100% 100%",

    [breakpoints.down("xs")]: {
      textAlign: "center",
      paddingBottom: "30px",
      background: `url(${
        palette.type === "dark"
          ? BACKGROUND_WAVE_BLUE_M
          : BACKGROUND_WAVE_WHITE_M
      }) no-repeat`,
      backgroundSize: "auto 100%",
      backgroundPosition: "right",
    },
  },

  container: {
    [breakpoints.down("xs")]: {
      textAlign: "center",
    },
  },

  description: {
    width: "700px",
    "& > div:first-child": {
      color: palette.primary.main,
      fontFamily: "Brandon Grotesque",
      fontWeight: 700,
      fontSize: "50px",
      paddingBottom: "20px",
      lineHeight: "110%",
      whiteSpace: "pre-line",

      [breakpoints.down("xs")]: {
        fontSize: "30px",
        whiteSpace: "unset",
      },
    },
    "& > div:last-child": {
      color: palette.primary.main,
      fontFamily: "Museo Sans",
      fontWeight: 100,
      fontSize: "20px",
      whiteSpace: "pre-line",

      [breakpoints.down("xs")]: {
        fontSize: "16px",
        whiteSpace: "unset",
      },
    },

    [breakpoints.down("xs")]: {
      width: "auto",
    },
  },

  coins: {
    "& > img": {
      marginRight: "20px",
      opacity: "0.5",
      [breakpoints.down("xs")]: {
        width: "20px",
        marginRight: "12px",
      },
    },
  },

  connectWallet: {
    cursor: "pointer",
    background: palette.info.light,
    padding: "10px 25px",
    fontFamily: "Brandon Grotesque",
    fontSize: "14px",
    color: palette.common.white,
    borderRadius: "100px",
    display: "inline-block",
    fontWeight: 700,
  },
}));

const AdSection: React.FC = () => {
  const { breakpoints } = useTheme();
  const dark = useIsDarkMode();
  const mobile = useMediaQuery(breakpoints.down("xs"));
  const classes = useStyles({ dark, mobile });

  return (
    <Box className={cx(classes.root)}>
      <Container className={cx(classes.container)}>
        <Grid
          container
          spacing={0}
          alignItems="center"
          direction={!mobile ? "row" : "column-reverse"}
        >
          <Grid item xs={12} sm={6}>
            <Box mx={!mobile ? "0px" : "20px"} mt={!mobile ? "0px" : "20px"}>
              <Box className={cx(classes.description)}>
                <Box>
                  {`Collateral assets can be leveraged\nto mint Ardana Stablecoins.`}
                </Box>
                <Box>
                  {`Open a Ardana Stablecoin Vault, deposit your\ncollateral, and generate dUSD against it.`}
                </Box>
              </Box>

              <Box mt="30px" />

              <Box ml={mobile ? "12px" : 0} className={cx(classes.coins)}>
                <img src={COIN_CARDANO} alt="cardano coin" />
                <img src={COIN_CARDANO} alt="cardano coin" />
                <img src={COIN_CARDANO} alt="cardano coin" />
                <img src={COIN_CARDANO} alt="cardano coin" />
                <img src={COIN_CARDANO} alt="cardano coin" />
                <img src={COIN_CARDANO} alt="cardano coin" />
                <img src={COIN_CARDANO} alt="cardano coin" />
              </Box>

              <Box mt={!mobile ? "20px" : "20px"} />

              <Box className={cx(classes.connectWallet)}>CONNECT A WALLET</Box>

              {mobile && (
                <Box mt="50px" textAlign="left">
                  <ThemeSwitch />
                </Box>
              )}
            </Box>
          </Grid>
          <Grid item xs={12} sm={6}>
            <Box display="flex" justifyContent="center">
              <img
                src={BACKGROUND_GRAPHIC}
                alt="graphic"
                width={!mobile ? "auto" : "80%"}
              />
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  );
};

export default AdSection;
