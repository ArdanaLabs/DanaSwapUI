import React from "react"
import { Box, Container, Grid, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { HelpCard } from "components"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
}))

const HelpSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Container>
        <Grid container spacing={5} alignItems="stretch">
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
