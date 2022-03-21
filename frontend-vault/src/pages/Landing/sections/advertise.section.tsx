import React from "react"
import { Theme, useTheme } from "@mui/system"
import { Box, Container, Grid, Typography, useMediaQuery } from "@mui/material"
import { makeStyles } from "@mui/styles"

import { useWallet } from "state/wallet/hooks"

import BACKGROUND_GRAPHIC from "assets/image/backgrounds/BG-GRAPHIC.png"
import BACKGROUND_WAVE_BLUE from "assets/image/backgrounds/hero-bg-dark.png"
import BACKGROUND_WAVE_WHITE from "assets/image/backgrounds/hero-bg-light.png"
import COIN_CARDANO from "assets/image/coins/cardano.png"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    paddingTop: "100px",
    paddingBottom: "50px",
    background: `url(${
      theme.palette.mode === "dark"
        ? BACKGROUND_WAVE_BLUE
        : BACKGROUND_WAVE_WHITE
    }) right bottom no-repeat`,
    backgroundSize: "cover",
    borderBottomRightRadius: "100px",

    [theme.breakpoints.down("sm")]: {
      textAlign: "center",
      paddingBottom: "30px",
      background: `url(${
        theme.palette.mode === "dark"
          ? BACKGROUND_WAVE_BLUE
          : BACKGROUND_WAVE_WHITE
      }) right bottom no-repeat`,
      borderBottomRightRadius: "75px",
    },
  },

  container: {
    [theme.breakpoints.down("sm")]: {
      textAlign: "center",
    },
  },

  description: {
    position: "relative",
    width: "660px",

    [`& > h1`]: {
      color: theme.palette.primary.main,
      whiteSpace: "pre-line",
    },
    [`& > h4`]: {
      color: theme.palette.primary.main,
      whiteSpace: "pre-line",
    },

    [theme.breakpoints.down("sm")]: {
      width: "auto",
    },
  },

  coins: {
    [`& > img`]: {
      marginRight: "20px",
      opacity: "0.5",
      [theme.breakpoints.down("sm")]: {
        width: "20px",
        marginRight: "12px",
      },
    },
  },

  connectWallet: {
    cursor: "pointer",
    background: theme.palette.info.light,
    padding: "10px 25px",
    color: theme.palette.common.white,
    borderRadius: "100px",
    display: "inline-block",
    textTransform: "uppercase",
  },
}))

const AdSection: React.FC = () => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))
  const classes = useStyles()
  const { address } = useWallet()

  return (
    <Box className={classes.root}>
      <Container className={classes.container}>
        <Grid
          container
          spacing={0}
          alignItems="center"
          direction={!mobile ? "row" : "column-reverse"}
        >
          <Grid item xs={12} sm={6}>
            <Box mx={!mobile ? "0px" : "20px"} mt={!mobile ? "0px" : "20px"}>
              <Box className={classes.description}>
                <Typography variant="h1">
                  {`Collateral assets can be leveraged\nto mint Ardana Stablecoins.`}
                </Typography>
                <Typography variant="h4">
                  {`Open a Ardana Stablecoin Vault, deposit your\ncollateral, and generate dUSD against it.`}
                </Typography>
              </Box>

              <Box mt="30px" />

              <Box ml={mobile ? "12px" : 0} className={classes.coins}>
                <img src={COIN_CARDANO} alt="cardano" />
                <img src={COIN_CARDANO} alt="cardano" />
                <img src={COIN_CARDANO} alt="cardano" />
                <img src={COIN_CARDANO} alt="cardano" />
                <img src={COIN_CARDANO} alt="cardano" />
                <img src={COIN_CARDANO} alt="cardano" />
                <img src={COIN_CARDANO} alt="cardano" />
              </Box>

              <Box mt={!mobile ? "20px" : "20px"} />

              {!address && (
                <Typography variant="h5" className={classes.connectWallet}>
                  Connect a wallet
                </Typography>
              )}
            </Box>
          </Grid>
          <Grid item xs={12} sm={6}>
            <Box display="flex" justifyContent="center">
              <img
                src={BACKGROUND_GRAPHIC}
                alt="graphic"
                width={!mobile ? "100%" : "80%"}
              />
            </Box>
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default AdSection
