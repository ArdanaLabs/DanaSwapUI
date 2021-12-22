import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"
import { useParams } from "react-router-dom"

import { useIsDarkMode } from "state/user/hooks"

const useStyles = makeStyles(({ palette, breakpoints }) => ({
  root: {},
}))

const StatSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  const { type } = useParams<{ type: string }>()

  return <Box className={cx(classes.root)}>{type}</Box>
}

export default StatSection
