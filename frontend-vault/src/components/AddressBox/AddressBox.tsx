import React from "react"
import cx from "classnames"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"

import { useWallet } from "state/wallet/hooks"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    padding: "5px",
    borderRadius: "100px",
    cursor: "pointer",
    border: `2px solid ${palette.primary.main}`,
    filter: "drop-shadow(0px 4px 4px rgba(0, 0, 0, 0.25))",
    width: "240px",
  },
  address: {},
  balance: {
    width: "100px",
  },
}))

const AddressBox: React.FC = () => {
  const theme = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(theme.breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })
  const { address, balance } = useWallet()

  return (
    <Box className={cx(classes.root)} display={"flex"} alignItems={"center"}>
      <Box className={cx(classes.address)}>{address}</Box>
      <Box className={cx(classes.balance)}>{balance}</Box>
    </Box>
  )
}

export default AddressBox
