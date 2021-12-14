import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { TransactionTable } from "components"

const useStyles = makeStyles(({ palette }) => ({
  root: {},
  label: {
    fontFamily: "Brandon Grotesque",
    fontStyle: "normal",
    fontWeight: 900,
    fontSize: "18px",
    lineHeight: "110%",
    color: palette.primary.main,
  },
}))

const TransactionsSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.label)}>Transactions</Box>
      <TransactionTable />
      {/* <TransactionGrid /> */}
    </Box>
  )
}

export default TransactionsSection
