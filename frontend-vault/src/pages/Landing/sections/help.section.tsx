import React from "react"

import { useTheme, Box, Container, Grid, useMediaQuery } from "@mui/material"

import { useIsDarkMode } from "state/user/hooks"
import { HelpCard } from "components"

const HelpSection: React.FC = () => {
  const theme = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))

  return (
    <Box>
      <Container>
        <Grid container spacing={!mobile ? 5 : 3} alignItems="stretch">
          <Grid item xs={12} sm={6}>
            <HelpCard
              title="Dana Coin"
              content={`Buy, send and manage your Dana Coin all\nin one place. Grow your Dana, and access\nplenty of providers.`}
              background={
                dark
                  ? "linear-gradient(180.2deg, #627EFF 0.17%, rgba(77, 97, 210, 0) 116.51%)"
                  : "linear-gradient(180deg, #2E49C5 0%, #6480FF 106.6%)"
              }
            />
          </Grid>
          <Grid item xs={12} sm={6}>
            <HelpCard
              title="Got questions?"
              content={`Learn more about Dana Coin, Danaswap\nand Stablecoin Vaults by visiting our\nFAQs page.`}
              background={
                dark
                  ? "linear-gradient(180deg, #71B5F3 0%, rgba(113, 206, 243, 0) 110%)"
                  : "linear-gradient(358.66deg, #72D2F2 -23.79%, #307BD3 107.77%)"
              }
            />
          </Grid>
        </Grid>
      </Container>
    </Box>
  )
}

export default HelpSection
