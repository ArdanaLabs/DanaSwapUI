import React from "react"
import { makeStyles } from "@mui/styles"
import { Box, Theme, useMediaQuery, useTheme } from "@mui/material"

import { Footer, Header } from "layouts"
import { ThemeSwitch } from "components"

const useStyles = makeStyles((theme: Theme) => ({
  root: {
    background: theme.palette.background.default,
  },
}))

const Layout: React.FC = ({ children }) => {
  const theme = useTheme()
  const mobile = useMediaQuery(theme.breakpoints.down("sm"))
  const classes = useStyles(theme)

  return (
    <Box className={classes.root}>
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
