import React from "react"

import { useWallet } from "state/wallet/hooks"

import WalletIcon from "assets/image/svgs/wallet.svg"
import { Box, Theme, Typography, useTheme } from "@mui/material"
import { makeStyles } from "@mui/styles"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    padding: "5px",
    borderRadius: "100px",
    cursor: "pointer",
    borderWidth: 3,
    borderStyle: "solid",
    borderColor: theme.palette.primary.main,
    filter: "drop-shadow(0px 4px 4px rgba(0, 0, 0, 0.25))",
    width: "220px",
    display: "flex",
    alignItems: "center",
    justifyContent: "center",

    [theme.breakpoints.down("sm")]: {
      width: "180px",
    },
  },
  address: {
    textAlign: "center",
    width: "100%",
    color: theme.palette.primary.main,

    [theme.breakpoints.down("sm")]: {
      fontSize: 10,
    },
  },
  balance: {
    width: "100px",
    background: theme.palette.info.main,
    borderRadius: "100px",

    [`& > .wallet`]: {
      background: theme.palette.info.light,
      borderRadius: "100px",
      padding: "5px",
      width: "34px",
      height: "34px",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",

      [theme.breakpoints.down("sm")]: {
        width: "30px",
        height: "30px",
      },
    },

    [`& > .amount`]: {
      color: theme.palette.common.white,
      marginRight: "20px",
      marginLeft: "10px",

      [theme.breakpoints.down("sm")]: {
        fontSize: 10,
      },
    },
  },
}))

const AddressCard: React.FC = () => {
  const theme = useTheme()
  const classes = useStyles(theme)
  const { address, balance } = useWallet()

  const smartTrim = (string: string, maxLength: number): string => {
    if (!string) return string
    if (maxLength < 1) return string
    if (string.length <= maxLength) return string
    if (maxLength === 1) return string.substring(0, 1) + "..."

    var midpoint = Math.ceil(string.length / 2)
    var toremove = string.length - maxLength
    var lstrip = Math.ceil(toremove / 2)
    var rstrip = toremove - lstrip
    return (
      string.substring(0, midpoint - lstrip) +
      "..." +
      string.substring(midpoint + rstrip)
    )
  }

  return (
    <Box className={classes.root}>
      <Typography variant="h6" component="h6" className={classes.address}>
        {smartTrim(address, 9)}
      </Typography>
      <Box className={classes.balance} display={"flex"} alignItems="center">
        <Box className="wallet">
          <img src={WalletIcon} alt="wallet" width="80%" />
        </Box>
        <Typography variant="h6" component="h6" className="amount">
          {balance}
        </Typography>
      </Box>
    </Box>
  )
}

export default AddressCard
