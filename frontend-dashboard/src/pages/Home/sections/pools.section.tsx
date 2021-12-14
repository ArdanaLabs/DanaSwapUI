import React from "react"
import { Box, useMediaQuery } from "@material-ui/core"
import { makeStyles, useTheme } from "@material-ui/core/styles"
import cx from "classnames"

import { useIsDarkMode } from "state/user/hooks"
import { PoolsPanel } from "components"

const useStyles = makeStyles(({ palette }) => ({
  self: {},
}))

const PoolsSection: React.FC = () => {
  const { breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.self)}>
      <PoolsPanel overView={true} />
    </Box>
  )
}

export default PoolsSection
