import { Box, Container, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { VaultStatCard, NoVaultStatCard } from "components/Card"
import React from "react"
import { useIsDarkMode } from "state/user/hooks"
import { useWallet } from "state/wallet/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    marginTop: "-150px",

    [breakpoints.down("xs")]: {
      marginTop: "30px",
    },
  },
}))

const VaultStatSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const { myVaults } = useWallet()

  return (
    <Box className={cx(classes.root)}>
      <Container>
        {myVaults.length > 0 && <VaultStatCard vaultList={myVaults} />}
        {myVaults.length === 0 && <NoVaultStatCard />}
      </Container>
    </Box>
  )
}

export default VaultStatSection