import React from "react"
import cx from "classnames"
import { Box } from "@material-ui/core"
import { makeStyles } from "@material-ui/core/styles"

import { ReactComponent as ArrowRightIcon } from "assets/image/icons/arrow-right.svg"
import { useWallet } from "state/wallet/hooks"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    "margin": "0 10px",
    "cursor": "pointer",
    "display": "flex",
    "alignItems": "center",
    "opacity": 1,

    "&:hover": {
      opacity: 0.75,
    },
  },
  label: {
    fontSize: "14px",
    fontWeight: 700,
    fontFamily: "Brandon Grotesque",
    color: palette.primary.main,
    textAlign: "center",
    lineHeight: "150%",
  },
  icon: {
    "marginLeft": "10px",

    "& path": {
      fill: palette.primary.main,
    },
  },
}))

const ConnectWallet: React.FC = () => {
  const classes = useStyles()
  const { updateWalletAddress, updateMyVaults } = useWallet()

  const handleConnectWallet = () => {
    updateWalletAddress("0x2ddA6C07ED3671F8d2f19B317e91e4DFD43f6621")
    updateMyVaults([
      {
        name: "YIFI1",
        image: require("assets/image/coins/dusd.svg").default,
        locked: 13794.18,
        debt: 5602.59,
        usdRate: 1.45,
        risk: false,
      },
      {
        name: "YIFI2",
        image: require("assets/image/coins/dusd.svg").default,
        locked: 13794.18,
        debt: 5602.59,
        usdRate: 1,
        risk: false,
      },
    ])
  }

  return (
    <Box className={cx(classes.root)} onClick={handleConnectWallet}>
      <Box className={cx(classes.label)}>CONNECT A WALLET</Box>
      <ArrowRightIcon className={cx(classes.icon)} />
    </Box>
  )
}

export default ConnectWallet
