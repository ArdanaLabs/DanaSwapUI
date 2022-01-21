import React from "react"
import cx from "classnames"
import { Box, Typography, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import { useIsDarkMode } from "state/user/hooks"

import WalletIcon from "assets/image/icons/wallet.svg"
import BigNumber from "bignumber.js"
import { Currencies } from "data"
import { currencyConvertor } from "hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    padding: "5px",
    borderRadius: "100px",
    cursor: "pointer",
    border: `2px solid ${palette.primary.main}`,
    filter: "drop-shadow(0px 4px 4px rgba(0, 0, 0, 0.25))",
    minWidth: "220px",

    [breakpoints.down("xs")]: {
      minWidth: "180px",
    },
  },
  address: {
    textAlign: "center",
    width: "100%",
    padding: "0 10px",
    color: palette.primary.main,

    [breakpoints.down("xs")]: {
      fontSize: 10,
    },
  },
  balance: {
    // minWidth: "100px",
    background: palette.info.main,
    borderRadius: "100px",

    [`& > .wallet`]: {
      background: palette.info.light,
      borderRadius: "100px",
      padding: "5px",
      width: "34px",
      height: "34px",

      [breakpoints.down("xs")]: {
        width: "30px",
        height: "30px",
      },
    },

    [`& > .amount`]: {
      color: palette.common.white,
      marginRight: "20px",
      marginLeft: "10px",

      [breakpoints.down("xs")]: {
        fontSize: 10,
      },
    },
  },
}))

interface Props {
  address: string
  balance: BigNumber
}

const AddressCard: React.FC<Props> = ({ address, balance }) => {
  const theme = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(theme.breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

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
    <Box
      className={cx(classes.root)}
      display={"flex"}
      alignItems={"center"}
      justifyContent={"center"}
    >
      <Typography variant="h6" component="h6" className={cx(classes.address)}>
        {smartTrim(address, 9)}
      </Typography>
      <Box className={cx(classes.balance)} display={"flex"} alignItems="center">
        <Box
          className="wallet"
          display={"flex"}
          alignItems="center"
          justifyContent={"center"}
        >
          <img src={WalletIcon} alt="wallet" width="80%" />
        </Box>
        <Typography variant="h6" component="h6" className="amount">
          {currencyConvertor(balance, Currencies.ada.decimals).toString()}
        </Typography>
      </Box>
    </Box>
  )
}

export default AddressCard
