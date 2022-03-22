import React from "react"

import { ReactComponent as ArrowRightIcon } from "assets/image/svgs/arrow-right.svg"
import { useWallet } from "state/wallet/hooks"
import { Box, Theme, Typography } from "@mui/material"
import { makeStyles } from "@mui/styles"
import { FontFamilies } from "theme"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    margin: "0 10px",
    cursor: "pointer",
    display: "flex",
    alignItems: "center",
    opacity: 1,

    [`&:hover`]: {
      opacity: 0.75,
    },
  },
  label: {
    fontSize: "14px",
    fontWeight: 700,
    fontFamily: FontFamilies.Brandon,
    color: theme.palette.primary.main,
    textAlign: "center",
    lineHeight: "150%",
    textTransform: "uppercase",
  },
  icon: {
    marginLeft: "10px",

    [`& path`]: {
      fill: theme.palette.primary.main,
    },
  },
}))

const ConnectWallet: React.FC = () => {
  const classes = useStyles()
  const { updateWalletAddress } = useWallet()

  const handleConnectWallet = () => {
    updateWalletAddress()
  }

  return (
    <Box className={classes.root} onClick={handleConnectWallet}>
      <Typography variant="h5" className={classes.label}>
        Connect a wallet
      </Typography>
      <ArrowRightIcon className={classes.icon} />
    </Box>
  )
}

export default ConnectWallet
