import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import * as PoolSetName from "Data/Pool/PoolSetName"
import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
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

export type Props = {
  poolSet: PoolSetName.Type
}

const TransactionsSection: React.FC<Props> = ({
  poolSet,
}: Props): JSX.Element => {
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

  return (
    <Box className={cx(classes.root)}>
      <Box className={cx(classes.label)}>Transactions</Box>
      <TransactionTable poolSet={poolSet} />
      {/* <TransactionGrid poolSet={poolSet}/> */}
    </Box>
  )
}

export default TransactionsSection
