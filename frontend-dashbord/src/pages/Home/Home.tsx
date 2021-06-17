import React from 'react'
import { Box, useMediaQuery } from '@material-ui/core'
import { makeStyles, useTheme } from '@material-ui/core/styles'

import { useIsDarkMode } from 'state/user/hooks'
import LandingImage from 'components/LandingImage'
import SwapForm from 'components/SwapForm'

const useStyles = makeStyles(({ palette }) => ({
  homeContainer: {
    height: '100%'
  }
}))

const Home: React.FC = () => {
  const { palette, breakpoints } = useTheme()
  const dark = useIsDarkMode()
  const mobile = useMediaQuery(breakpoints.down('xs'))
  const classes = useStyles({ dark, mobile })

  return (
    <Box style={mobile ? { backgroundColor: palette.background.paper } : {}}>
      <LandingImage url={'HOME'} title={'Home'} />
      <SwapForm />
    </Box>
  )
}

export default Home
