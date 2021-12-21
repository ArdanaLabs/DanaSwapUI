import {
  Box,
  makeStyles,
  Typography,
  useMediaQuery,
  useTheme,
} from "@material-ui/core"
import React from "react"
import cx from "classnames"
import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    "background":
      palette.type === "dark"
        ? palette.background.paper
        : "linear-gradient(359deg, #B9D4FF -129.98%, #FFFFFF 99.14%)",
    "borderRadius": "20px",
    "padding": "50px",

    "& h1, & h6": {
      color: palette.primary.main,
    },
    "& h5": {
      color: palette.common.white,
      lineHeight: "100% !important",
    },

    [breakpoints.down("xs")]: {
      textAlign: "center",
      padding: "100px 10px",
    },
  },

  openVault: {
    background: palette.info.light,
    textTransform: "uppercase",
    padding: "10px 20px",
    borderRadius: "100px",
    cursor: "pointer",
  },
}))

const NoVaultStatCard = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const handleOpenVault = () => {
    console.log("handle")
  }

  return (
    <Box
      className={cx(classes.root)}
      display={"flex"}
      flexDirection={"column"}
      justifyContent={"center"}
      alignItems={"center"}
    >
      <Typography variant="h1" component="h1">
        You have no open Vaults
      </Typography>

      <Box mb="10px" />

      <Typography variant="h6" component="h6">
        Open a Maker Vault, deposit your collateral, and borrow Dai or buy
        additional collateral against it.
      </Typography>

      <Box mb="30px" />

      <Box className={cx(classes.openVault)} onClick={handleOpenVault}>
        <Typography variant="h5" component="h5">
          Open Vault
        </Typography>
      </Box>
    </Box>
  )
}
export default NoVaultStatCard
