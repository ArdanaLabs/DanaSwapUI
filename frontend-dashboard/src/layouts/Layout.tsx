import React from "react"
import { Box, makeStyles, useMediaQuery, useTheme } from "@material-ui/core"

import * as Theme from "Data/User/Theme"

import { useUserTheme } from "state/user/hooks"
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
  const { breakpoints } = useTheme()
  const userTheme: Theme.Theme = useUserTheme()
  const mobile = useMediaQuery(breakpoints.down("xs"))
  const classes = useStyles({
    dark: Theme.Eq.equals(userTheme, Theme.Theme.Dark),
    mobile,
  })

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
