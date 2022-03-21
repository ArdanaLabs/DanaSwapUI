import { Box, Container, useTheme, Theme } from "@mui/material"
import { makeStyles } from "@mui/styles"
import { VaultStatCard, NoVaultStatCard } from "components/Card"
import React from "react"
import { useWallet } from "state/wallet/hooks"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    marginTop: "-150px",

    [theme.breakpoints.down("sm")]: {
      marginTop: "30px",
    },
  },
}))

const VaultStatSection: React.FC = () => {
  const theme = useTheme()
  const classes = useStyles(theme)

  const { myVaults } = useWallet()

  return (
    <Box className={classes.root}>
      <Container>
        {myVaults.length > 0 && <VaultStatCard vaultList={myVaults} />}
        {myVaults.length === 0 && <NoVaultStatCard />}
      </Container>
    </Box>
  )
}

export default VaultStatSection
