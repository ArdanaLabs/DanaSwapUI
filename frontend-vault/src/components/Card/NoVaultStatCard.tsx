import React from "react"
import { Box, Theme, Typography, useTheme } from "@mui/material"
import { makeStyles } from "@mui/styles"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    display: "flex",
    flexDirection: "column",
    justifyContent: "center",
    alignItems: "center",
    background:
      theme.palette.mode === "dark"
        ? theme.palette.background.paper
        : "linear-gradient(359deg, #B9D4FF -129.98%, #FFFFFF 99.14%)",
    borderRadius: "20px",
    padding: "50px",
    color: theme.palette.primary.main,

    [`& h1`]: {
      color: "currentColor",
      marginBottom: "10px",
    },
    [`& h6`]: {
      color: "currentColor",
      marginBottom: "30px",
    },
    [`& h5`]: {
      color: theme.palette.common.white,
      lineHeight: "100% !important",
    },

    [theme.breakpoints.down("sm")]: {
      textAlign: "center",
      padding: "100px 10px",
    },
  },

  openVault: {
    background: theme.palette.info.light,
    textTransform: "uppercase",
    padding: "10px 20px",
    borderRadius: "100px",
    cursor: "pointer",
  },
}))

const NoVaultStatCard = () => {
  const theme = useTheme()
  const classes = useStyles(theme)

  const handleOpenVault = () => {
    console.log("handle")
  }

  return (
    <Box className={classes.root}>
      <Typography variant="h1" component="h1">
        You have no open Vaults
      </Typography>

      <Typography variant="h6" component="h6">
        Open a Maker Vault, deposit your collateral, and borrow Dai or buy
        additional collateral against it.
      </Typography>

      <Box className={classes.openVault} onClick={handleOpenVault}>
        <Typography variant="h5" component="h5">
          Open Vault
        </Typography>
      </Box>
    </Box>
  )
}
export default NoVaultStatCard
