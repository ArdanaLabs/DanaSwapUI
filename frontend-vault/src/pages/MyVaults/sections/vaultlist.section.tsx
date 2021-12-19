import { Box, Container, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import React from "react"
import { useIsDarkMode } from "state/user/hooks"
import { useWallet } from "state/wallet/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {
    margin: "30px 0px",
  },
}))

const VaultListSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const { myVaults } = useWallet()

  return (
    <Box className={cx(classes.root)}>
      <Container>{myVaults.length > 0 && <Box>asdf</Box>}</Container>
    </Box>
  )
}

export default VaultListSection
