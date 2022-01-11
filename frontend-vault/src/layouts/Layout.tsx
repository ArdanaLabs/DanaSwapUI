import React from "react"
import cx from "classnames"
import { Box, makeStyles, useMediaQuery, useTheme } from "@material-ui/core"
import { useIsDarkMode } from "state/user/hooks"
import { Footer, Header } from "layouts"
import { ThemeSwitch } from "components"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: palette.background.default,
  },
}))

const Layout: React.FC = ({ children }) => {
  const dark = useIsDarkMode()
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <Box className={cx(classes.root)}>
      <Header />
      <Box>{children}</Box>
      <Footer />

      {mobile && (
        <Box position="fixed" left={10} bottom={30}>
          <ThemeSwitch />
        </Box>
      )}
    </Box>
  )
}

export default Layout
