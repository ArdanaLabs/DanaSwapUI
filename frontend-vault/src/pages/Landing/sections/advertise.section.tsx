import React from "react";
import { Box, Container, useMediaQuery } from "@material-ui/core";
import { makeStyles, useTheme } from "@material-ui/core/styles";
import cx from "classnames";

import { useIsDarkMode } from "state/user/hooks";
import { ThemeSwitch } from "components";

import BACKGROUND_GRAPHIC from "assets/image/backgrounds/BG-GRAPHIC.png";
import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/BG-BLUE.png";
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/BG-WHITE.png";
import COIN_CARDANO from "assets/image/COIN1.png";

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    background: `url(${
      palette.type === "dark" ? BACKGROUND_WAVE_BLUE : BACKGROUND_WAVE_WHITE
    }) no-repeat`,
    backgroundSize: "100% 100%",

    [breakpoints.down("xs")]: {
      textAlign: "center",
    },
  },

  container: {
    background: `url(${BACKGROUND_GRAPHIC}) right top no-repeat`,

    [breakpoints.down("xs")]: {
      backgroundSize: "contain",
      textAlign: "center",
    },
  },

  description: {
    "& > div:first-child": {
      color: palette.primary.main,
      fontFamily: "Brandon Grotesque",
      fontWeight: 700,
      fontSize: "50px",
      paddingBottom: "20px",
      lineHeight: "110%",

      [breakpoints.down("xs")]: {
        fontSize: "30px",
      },
    },
    "& > div:last-child": {
      color: palette.primary.main,
      fontFamily: "Museo Sans",
      fontWeight: 100,
      fontSize: "20px",

      [breakpoints.down("xs")]: {
        fontSize: "16px",
      },
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
    background: palette.info.light,
    padding: "5px 40px",
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
        <Box pt={!mobile ? "100px" : "350px"} />

        <Box className={cx(classes.description)}>
          <Box>
            Collateral assets can be leveraged to
            <br /> mint Ardana Stablecoins.
          </Box>
          <Box>
            Open a Ardana Stablecoin Vault, deposit your
            <br /> collateral, and generate dUSD against it.
          </Box>
        </Box>

        <Box mt="50px" />

        <Box ml={mobile ? "12px" : 0} className={cx(classes.coins)}>
          <img src={COIN_CARDANO} alt="cardano coin" />
          <img src={COIN_CARDANO} alt="cardano coin" />
          <img src={COIN_CARDANO} alt="cardano coin" />
          <img src={COIN_CARDANO} alt="cardano coin" />
          <img src={COIN_CARDANO} alt="cardano coin" />
          <img src={COIN_CARDANO} alt="cardano coin" />
          <img src={COIN_CARDANO} alt="cardano coin" />
        </Box>

        <Box mt={!mobile ? "40px" : "20px"} />

        <Box className={cx(classes.connectWallet)}>CONNECT A WALLET</Box>

        {mobile && (
          <Box mt="50px" textAlign="left">
            <ThemeSwitch />
          </Box>
        )}
      </Container>
    </Box>
  );
};

export default AdSection;
