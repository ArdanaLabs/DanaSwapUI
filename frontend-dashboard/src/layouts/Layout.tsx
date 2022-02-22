import React from "react"
import { Box, makeStyles, useMediaQuery, useTheme } from "@material-ui/core"
import { useIsDarkMode } from "state/user/hooks"
import { Footer, Header } from "layouts"

const useStyles = makeStyles(({ palette }) => ({
  root: {
    background: palette.background.default,
  },
}))

export interface LayoutProps {
  children: any
}

const Layout: React.FC<LayoutProps> = ({ children }) => {
  const dark = useIsDarkMode()
  const { breakpoints } = useTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({ dark, mobile })

  return (
    <>
      <Header />
      <Box className={classes.root}>
        {children}
        <Footer />
      </Box>
    </>
  )
}

export default Layout
